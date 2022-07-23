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

module Main (main) where

import Brick
import Brick.AttrMap qualified as A
import Brick.BChan
  ( BChan,
    newBChan,
    readBChan,
    writeBChan,
  )
import Brick.Focus
  ( focusGetCurrent,
    focusRingCursor,
  )
import Brick.Forms
  ( Form,
    allFieldsValid,
    checkboxField,
    editPasswordField,
    editShowableField,
    editTextField,
    focusedFormInputAttr,
    formFocus,
    formState,
    handleFormEvent,
    invalidFields,
    invalidFormInputAttr,
    newForm,
    radioField,
    renderForm,
    setFieldValid,
    (@@=),
  )
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Buttplug.Core (Device (..), Message (..), Vibrate (..), clientMessageVersion)
import Buttplug.Core.Handle qualified as Buttplug
import Buttplug.Core.WebSockets qualified as BPWS
import Control.Arrow ((>>>))
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Char (digitToInt, isDigit, ord)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Semigroup (First (..))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Ki
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH
import Streamly.Prelude (IsStream, nil, (|:))
import Streamly.Prelude qualified as S
import System.Environment
import System.Exit (exitFailure)
import System.Process

data ConnectScreenName
  = HostField
  | PortField
  deriving (Eq, Ord, Show)

data HostPort = HostPort {_host :: String, _port :: Int}
  deriving (Show, Eq)

makeLenses ''HostPort

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.black),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (invalidFormInputAttr, V.white `on` V.red),
      (focusedFormInputAttr, V.black `on` V.yellow),
      ("header", V.black `on` V.white),
      (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.white `on` V.blue)
    ]

data VibeMenuName
  = MessageLog
  | DeviceMenu
  deriving (Eq, Ord, Show)

-- Commands from the UI thread to the background thread
data ButtplugCommand
  = CmdStopAll
  | CmdVibrate Word Double
  deriving (Show)

-- TODO give BPWS.Connector a `Show` instance so we can derive `Show` here
data Command
  = BPCommand ButtplugCommand
  | CmdConnect BPWS.Connector

data VibeMenuState = VibeMenuState
  { _messageLog :: L.List VibeMenuName Message,
    _devices :: L.List VibeMenuName Device,
    _cmdChan :: BChan Command
  }

makeLenses ''VibeMenuState

-- events from bg thread to UI thread
data VibeMenuEvent = EvConnected | BPEvent BPSessionEvent

-- events from buttplug server
data BPSessionEvent
  = ReceivedMessage Message
  | ReceivedDeviceList [Device]
  | EvDeviceAdded Device
  | EvDeviceRemoved Word

drawVibeMenu s = [ui]
  where
    header = withAttr "header" . txtWrap
    title = padBottom (Pad 1) $ header "VibeMenu"
    deviceMenu =
      header "Connected Devices"
        <=> padBottom (Pad 1) (L.renderList listDrawDevice True $ s ^. devices)
    receivedMsgLog =
      header "Message log"
        <=> padBottom (Pad 1) (L.renderList listDrawElement False $ s ^. messageLog)

    ui =
      title
        <=> deviceMenu
        <=> receivedMsgLog

listDrawDevice :: Bool -> Device -> Widget VibeMenuName
listDrawDevice sel Device {..}
  | sel = withAttr L.listSelectedAttr $ txt label
  | otherwise = txt label
  where
    label :: T.Text
    label = T.pack (show deviceIndex) <> " " <> deviceName

listDrawElement :: (Show e) => Bool -> e -> Widget VibeMenuName
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr L.listSelectedAttr $ str s
          else str s
   in (selStr $ show a)

vibeMenu ::
  (VibeMenuState -> EventM VibeMenuName VibeMenuState) ->
  App VibeMenuState VibeMenuEvent VibeMenuName
vibeMenu startEvent =
  App
    { appDraw = drawVibeMenu,
      appHandleEvent = vibeMenuHandleEvent,
      appChooseCursor = neverShowCursor, -- TODO
      appStartEvent = startEvent,
      appAttrMap = const theMap
    }

vibeMenuHandleEvent ::
  VibeMenuState ->
  BrickEvent VibeMenuName VibeMenuEvent ->
  EventM VibeMenuName (Next VibeMenuState)
vibeMenuHandleEvent s = \case
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
    e -> handleEventLensed s devices L.handleListEvent e >>= continue
  AppEvent (BPEvent e) -> case e of
    ReceivedMessage msg ->
      continue $
        s
          & messageLog
            %~ (listAppend msg >>> L.listMoveToEnd)
    ReceivedDeviceList devs ->
      continue $ s & devices .~ L.list DeviceMenu (Vec.fromList devs) 1
    EvDeviceAdded dev@(Device devName (fromIntegral -> ix) _) ->
      continue $ s & devices %~ listAppend dev
    EvDeviceRemoved (fromIntegral -> ix) ->
      continue $ s & devices %~ deleteDeviceByIndex ix
  AppEvent EvConnected -> continue s --todo
  _ -> continue s
  where
    sendCommand :: Command -> EventM n ()
    sendCommand = liftIO . writeBChan (s ^. cmdChan)

    selectedDevice = s ^. devices & L.listSelectedElement

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

listAppend :: e -> L.List n e -> L.List n e
listAppend elt l =
  let n = Vec.length $ L.listElements l
   in L.listInsert n elt l

deleteDeviceByIndex :: Word -> L.List n Device -> L.List n Device
deleteDeviceByIndex devIx l =
  case Vec.findIndex ((devIx ==) . deviceIndex) (L.listElements l) of
    Just listIndex -> L.listRemove listIndex l
    Nothing -> l

-- TODO switch to optparse applicative maybe
getHostPort :: IO HostPort
getHostPort = do
  args <- getArgs
  pure $ case args of
    [host, port] -> HostPort host (read port)
    [port] -> HostPort defaultHost (read port)
    [] -> HostPort defaultHost defaultPort
  where
    defaultHost = "127.0.0.1"
    defaultPort = 12345

buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

main :: IO ()
main = do
  HostPort host port <- getHostPort
  cmdChan <- newBChan 30

  vty <- buildVty

  let connector = BPWS.Connector host port
      initialState =
        VibeMenuState
          (L.list MessageLog mempty 1)
          (L.list DeviceMenu mempty 1)
          cmdChan

      startEvent s = do
        liftIO $ writeBChan cmdChan (CmdConnect connector) 
        pure s

  evChan :: BChan VibeMenuEvent <- newBChan 10
  scoped \scope -> do
    fork scope $ workerThread connector cmdChan evChan
    uiThread <-
      fork scope $
        customMain vty buildVty (Just evChan) (vibeMenu startEvent) initialState
    atomically $ await uiThread
  pure ()

-- Main background thread
workerThread :: BPWS.Connector -> BChan Command -> BChan VibeMenuEvent -> IO ()
workerThread connector cmdChan evChan = do
  buttplugCmdChan <- newBChan 30
  scoped \scope -> do
    forever $
      readBChan cmdChan >>= \case
        -- for now connection happens once automatically at the start of the program
        CmdConnect _ -> do
          fork scope $ connect connector buttplugCmdChan evChan
          pure ()
        BPCommand bpCmd -> writeBChan buttplugCmdChan bpCmd

-- Background thread which handles communication with the server
connect :: BPWS.Connector -> BChan ButtplugCommand -> BChan VibeMenuEvent -> IO ()
connect connector buttplugCmdChan evChan = do
  putStrLn $
    "Connecting to: " <> connector.wsConnectorHost <> ":" <> show connector.wsConnectorPort
  -- TODO handle exceptions
  BPWS.runClient connector \handle -> do
    putStrLn "connected!"
    writeBChan evChan EvConnected
    sendReceiveBPMessages handle evChan buttplugCmdChan

sendReceiveBPMessages ::
  Buttplug.Handle ->
  BChan VibeMenuEvent ->
  BChan ButtplugCommand ->
  IO ()
sendReceiveBPMessages handle evChan buttplugCmdChan = do
  -- TODO factor out handleshake
  -- block while we perform handshake
  Buttplug.sendMessage handle $ MsgRequestServerInfo 1 "VibeMenu" clientMessageVersion
  [servInfo@(MsgServerInfo 1 _ _ _)] <- Buttplug.receiveMessages handle
  emitBPEvent $ ReceivedMessage servInfo

  scoped \scope -> do
    fork scope $ Buttplug.sendMessage handle $ MsgRequestDeviceList 2
    fork scope $ Buttplug.sendMessage handle $ MsgStartScanning 3
    -- main loop
    fork scope $
      S.mapM_ emitBPEvent $
        S.concatMap (S.fromFoldable . toEvents) $
          buttplugMessages handle
    fork scope $
      S.mapM_ (handleButtplugCommand handle) $
        (S.repeatM . readBChan) buttplugCmdChan

    atomically $ awaitAll scope
  where
    emitBPEvent = writeBChan evChan . BPEvent

-- forward messages from the UI to the buttplug server
handleButtplugCommand :: Buttplug.Handle -> ButtplugCommand -> IO ()
handleButtplugCommand con = \case
  CmdStopAll -> Buttplug.sendMessage con $ MsgStopAllDevices 1
  CmdVibrate devIx speed ->
    Buttplug.sendMessage con $
      MsgVibrateCmd 1 devIx [Vibrate 0 speed]

-- Produces all messages that come in through a buttplug connection
buttplugMessages ::
  IsStream t =>
  Buttplug.Handle ->
  t IO Message
-- buttplugMessages con = forever $ lift (receiveMsgs con) >>= each
buttplugMessages con =
  S.repeatM (Buttplug.receiveMessages con)
    & S.concatMap S.fromFoldable

-- We notify the UI of every message so it can display them, but also
-- translate messages into simplified events
toEvents :: Message -> [BPSessionEvent]
toEvents msg =
  catMaybes
    [ Just $ ReceivedMessage msg,
      msgToBPSessionEvent msg
    ]
  where
    -- Translate messages that the UI needs to know about to events, discarding
    -- the unnecessary ones
    msgToBPSessionEvent :: Message -> Maybe BPSessionEvent
    msgToBPSessionEvent = \case
      MsgDeviceAdded _ name ix devmsgs -> Just $ EvDeviceAdded $ Device name ix devmsgs
      MsgDeviceRemoved _ ix -> Just $ EvDeviceRemoved ix
      MsgDeviceList _ devices -> Just $ ReceivedDeviceList devices
      _ -> Nothing
