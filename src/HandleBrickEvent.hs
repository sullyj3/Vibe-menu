{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module HandleBrickEvent (vibeMenuHandleEvent) where

import Brick
import Brick.BChan (writeBChan)
import Brick.Forms
  ( allFieldsValid,
    formState,
    handleFormEvent,
    setFieldValid,
  )
import Brick.Widgets.List qualified as L
import Buttplug.Core (Device (..))
import Control.Arrow ((>>>))
import Control.Monad.IO.Class
import Data.Char (digitToInt, isDigit)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (.~), (^.))
import System.IO
import Types

connectScreenHandleEvent ::
  AppState ->
  BrickEvent VibeMenuName VibeMenuEvent ->
  EventM VibeMenuName (Next AppState)
connectScreenHandleEvent s = \case
  VtyEvent V.EvResize {} -> continue s
  VtyEvent (V.EvKey V.KEsc []) -> halt s
  VtyEvent (V.EvKey V.KEnter []) -> do
    let connectForm = s ^. appScreenState . connectScreenState
    if allFieldsValid connectForm
      then do
        let connector = hostPortToConnector $ formState connectForm
        liftIO $ writeBChan (s ^. appCmdChan) $ CmdConnect connector
        continue $ s & appScreenState .~ ConnectingScreen
      else do
        -- TODO signal error to user in ui
        liftIO $ hPutStrLn stderr "Invalid form, check the port is a number."
        continue s
  ev -> do
    continue
      =<< handleEventLensed s (appScreenState . connectScreenState) updateForm ev
  where
    updateForm ev form = do
      form' :: ConnectForm <- handleFormEvent ev form
      let portValid = formState form' ^. port >= 0
      pure $ setFieldValid portValid PortField form'

vibeMenuHandleEvent ::
  AppState ->
  BrickEvent VibeMenuName VibeMenuEvent ->
  EventM VibeMenuName (Next AppState)
vibeMenuHandleEvent s@(AppState _ screen) = handle s
  where
    handle = case screen of
      ConnectScreen _ -> connectScreenHandleEvent
      ConnectingScreen -> connectingScreenHandleEvent
      MainScreen _ -> mainScreenHandleEvent

connectingScreenHandleEvent ::
  AppState ->
  BrickEvent VibeMenuName VibeMenuEvent ->
  EventM VibeMenuName (Next AppState)
connectingScreenHandleEvent s = \case
  AppEvent EvConnected -> continue $ s & appScreenState .~ MainScreen initialMainScreenState
  _ -> continue s
  where
    initialMainScreenState =
      MainScreenState
        (L.list MessageLog mempty 1)
        (L.list DeviceMenu mempty 1)

mainScreenHandleEvent ::
  AppState ->
  BrickEvent VibeMenuName VibeMenuEvent ->
  EventM VibeMenuName (Next AppState)
mainScreenHandleEvent s = \case
  VtyEvent e -> case e of
    V.EvResize {} -> continue s
    V.EvKey V.KEsc [] -> halt s
    V.EvKey (V.KChar c) [] -> case c of
      's' -> do
        sendCommand $ BPCommand CmdStopAll
        continue s
      'q' -> halt s
      _
        | isDigit c -> do
            withSelectedDevice $ vibratePercent (outOfTen . digitToInt $ c)
            continue s
        | otherwise -> continue s
    vtyEvent -> continue =<<
      handleEventLensed
        s
        (appScreenState . mainScreenState . devices)
        L.handleListEvent
        vtyEvent
  AppEvent (BPEvent e) -> case e of
    ReceivedMessage msg ->
      continue $
        s
          & appScreenState . mainScreenState . messageLog
            %~ (listAppend msg >>> L.listMoveToEnd)
    ReceivedDeviceList devs ->
      continue $ s & appScreenState . mainScreenState . devices .~ L.list DeviceMenu (Vec.fromList devs) 1
    EvDeviceAdded dev@(Device _devName _ix _) ->
      continue $ s & appScreenState . mainScreenState . devices %~ listAppend dev
    EvDeviceRemoved (fromIntegral -> ix) ->
      continue $ s & appScreenState . mainScreenState . devices %~ deleteDeviceByIndex ix
  AppEvent EvConnected -> continue s -- todo
  _ -> continue s
  where
    sendCommand :: Command -> EventM n ()
    sendCommand = liftIO . writeBChan (s ^. appCmdChan)

    selectedDevice = s ^. appScreenState . mainScreenState . devices & L.listSelectedElement

    withSelectedDevice ::
      (Int -> Device -> EventM VibeMenuName ()) ->
      EventM VibeMenuName ()
    withSelectedDevice k = case selectedDevice of
      Just (index, device) -> k index device
      Nothing -> pure ()

    vibratePercent :: Double -> Int -> Device -> EventM n ()
    vibratePercent speed _ (Device _ devIndex _) = do
      sendCommand $ BPCommand $ CmdVibrate devIndex speed

    -- convert a character digit between 1 and 9, to a float between 0.1 and 0.9
    -- we also allow 0 to represent 10, mapping to 1. The idea is to use the
    -- numeric row on a keyboard to indicate intensity
    outOfTen = \case
      0 -> 1
      n -> fromIntegral n / 10
