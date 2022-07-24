{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View (drawVibeMenu, theMap) where

import Brick
    ( attrMap,
      on,
      (<=>),
      hLimit,
      padBottom,
      padTop,
      str,
      txt,
      txtWrap,
      withAttr,
      AttrMap,
      Padding(Pad),
      Widget )
import Brick.Forms (focusedFormInputAttr, invalidFormInputAttr, renderForm)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.List qualified as L
import Buttplug.Core (Device (..))
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Types
    ( VibeMenuName,
      AppState,
      ScreenState(MainScreen, ConnectScreen, ConnectingScreen),
      devices,
      messageLog,
      mainScreenState,
      connectScreenState,
      appScreenState )

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

drawVibeMenu :: AppState -> [Widget VibeMenuName]
drawVibeMenu s = case s ^. appScreenState of
  ConnectScreen _ -> drawConnectScreen s
  ConnectingScreen _ -> drawConnectingScreen s
  MainScreen _ -> drawMainScreen s

drawConnectScreen :: AppState -> [Widget VibeMenuName]
drawConnectScreen s = [C.vCenter $ C.hCenter form]
  where
    f = s ^. appScreenState . connectScreenState
    form = B.border $ padTop (Pad 1) $ hLimit 40 $ renderForm f

drawConnectingScreen :: AppState -> [Widget VibeMenuName]
drawConnectingScreen _ = [txt "Connecting..."]

drawMainScreen :: AppState -> [Widget VibeMenuName]
drawMainScreen appState = [ui]
  where
    s = appState ^. appScreenState . mainScreenState
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
