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

module Types
  ( AppState (..),
    appCmdChan,
    ScreenState (..),
    VibeMenuName (..),
    VibeMenuEvent (..),
    host,
    port,
    appScreenState,
    MainScreenState (..),
    mainScreenState,
    listAppend,
    deleteDeviceByIndex,
    hostPortToConnector,
    messageLog,
    devices,
    connectScreenState,
    connectingScreenState,
    Command (..),
    BPSessionEvent (..),
    mkConnectForm,
    HostPort (..),
    ConnectForm,
    ButtplugCommand (..),
  )
where

import Brick
import Brick.BChan (BChan)
import Brick.Forms (Form, editShowableField, editTextField, newForm, (@@=))
import Brick.Widgets.List qualified as L
import Buttplug.Core (Device (..))
import Buttplug.Core.WebSockets qualified as BPWS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vec
import Lens.Micro (Lens', lens)
import Lens.Micro.TH
import Buttplug.Message

data HostPort = HostPort {_host :: Text, _port :: Int}
  deriving (Show, Eq)

makeLenses ''HostPort

data VibeMenuName
  = MessageLog
  | DeviceMenu
  | HostField
  | PortField
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

-- events from bg thread to UI thread
data VibeMenuEvent = EvConnected | BPEvent BPSessionEvent

-- events from buttplug server
data BPSessionEvent
  = ReceivedMessage Message
  | ReceivedDeviceList [Device]
  | EvDeviceAdded Device
  | EvDeviceRemoved Word

data MainScreenState = MainScreenState
  { _messageLog :: L.List VibeMenuName Message,
    _devices :: L.List VibeMenuName Device
  }

makeLenses ''MainScreenState

type ConnectForm = Form HostPort VibeMenuEvent VibeMenuName

mkConnectForm :: HostPort -> ConnectForm
mkConnectForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Host"
            @@= editTextField host HostField (Just 1),
          label "Port"
            @@= editShowableField port PortField
        ]

data ScreenState
  = ConnectScreen ConnectForm
  | ConnectingScreen (BPWS.Connector)
  | MainScreen MainScreenState

data AppState = AppState {_appCmdChan :: BChan Command, _appScreenState :: ScreenState}

makeLenses ''AppState

mainScreenState :: Lens' ScreenState MainScreenState
mainScreenState = lens get set
  where
    get = \case
      MainScreen mss -> mss
      _ -> error "BUG: mainScreenState: tried to get MainScreenState from incorrect constructor"

    set appState mss = case appState of
      MainScreen _ -> MainScreen mss
      _ ->
        error
          "BUG: mainScreenState: tried to write MainScreenState to incorrect constructor field"

connectScreenState :: Lens' ScreenState ConnectForm
connectScreenState = lens get set
  where
    get = \case
      ConnectScreen connectForm -> connectForm
      _ -> error "BUG: connectScreenState: tried to get ConnectForm from incorrect constructor"

    set appState connectForm = case appState of
      ConnectScreen _ -> ConnectScreen connectForm
      _ ->
        error
          "BUG: connectScreenState: tried to write ConnectForm to incorrect constructor field"

connectingScreenState :: Lens' ScreenState BPWS.Connector
connectingScreenState = lens get set
  where
    get = \case
      ConnectingScreen connector -> connector
      _ -> error "BUG: connectingScreenState: tried to get Connector from incorrect constructor"

    set appState connector = case appState of
      ConnectingScreen _ -> ConnectingScreen connector
      _ ->
        error
          "BUG: connectingScreenState: tried to write Connector to incorrect constructor field"

listAppend :: e -> L.List n e -> L.List n e
listAppend elt l =
  let n = Vec.length $ L.listElements l
   in L.listInsert n elt l

deleteDeviceByIndex :: Word -> L.List n Device -> L.List n Device
deleteDeviceByIndex devIx l =
  case Vec.findIndex ((devIx ==) . deviceIndex) (L.listElements l) of
    Just listIndex -> L.listRemove listIndex l
    Nothing -> l

hostPortToConnector :: HostPort -> BPWS.Connector
hostPortToConnector (HostPort h p) = BPWS.Connector (T.unpack h) p
