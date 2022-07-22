{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

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
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Char (isDigit, ord)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Flow
import Graphics.Vty qualified as V
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH
import Streamly hiding ((<=>))
import Streamly.Internal.Network.Inet.TCP (connect)
import Streamly.Prelude (nil, (|:))
import Streamly.Prelude qualified as S
import System.Environment
import System.Exit (exitFailure)
import System.IO
import System.Process

-- This is needed because brick's editTextField widget uses Text rather than String
-- That means we can't use a Connector as the Form state, because it uses string
data HostPort = HostPort {_host :: T.Text, _port :: Int}
  deriving (Show, Eq)

makeLenses ''HostPort

data VibeMenuName
  = MessageLog
  | DeviceMenu
  | HostField
  | PortField
  deriving (Eq, Ord, Show)

-- Commands from the UI thread to the background thread
data Command
  = CmdConnect BPWS.Connector
  | CmdButtplug ButtplugCommand
  deriving (Show)

data ButtplugCommand
  = CmdStopAll
  | CmdVibrate Word Double
  deriving (Show)

-- events from bg thread to UI thread
data CustomEvent
  = ReceivedMessage Message
  | ReceivedDeviceList [Device]
  | EvDeviceAdded Device
  | EvDeviceRemoved Word
  | EvConnected

data AppState = AppState
  { _cmdChan :: BChan Command,
    _appScreen :: ScreenState
  }

type ConnectScreenState = Form HostPort CustomEvent VibeMenuName

data ScreenState
  = ConnectScreenState ConnectScreenState
  | ConnectingScreenState
  | MainScreenState VibeMenuState

data VibeMenuState = VibeMenuState
  { _messageLog :: L.List VibeMenuName Message,
    _devices :: L.List VibeMenuName Device
  }

makeLenses ''VibeMenuState

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: HostPort -> Form HostPort e VibeMenuName
mkForm =
  let label s w =
        padBottom (Pad 1) $
          vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
   in newForm
        [ label "Host"
            @@= editTextField host HostField (Just 1),
          label "Port"
            @@= editShowableField port PortField
        ]

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

connectScreenHandleEvent ::
  ConnectScreenState ->
  BChan Command ->
  BrickEvent VibeMenuName CustomEvent ->
  EventM VibeMenuName (Next AppState)
connectScreenHandleEvent s cmdChan ev =
  case ev of
    VtyEvent V.EvResize {} -> continue sameState
    VtyEvent (V.EvKey V.KEsc []) -> halt sameState
    VtyEvent (V.EvKey V.KEnter []) -> do
      if allFieldsValid s then do
        let HostPort host port = formState s
            connector = BPWS.Connector (T.unpack host) port
        liftIO $ writeBChan cmdChan $ CmdConnect connector
        continue $ AppState cmdChan ConnectingScreenState
      else do
        -- TODO signal error to user in ui
        liftIO $ hPutStrLn stderr "Invalid form, check the port is a number."
        continue sameState
    _ -> do
      form' <- handleFormEvent ev s

      -- Example of external validation:
      continue $
        connectState $
          setFieldValid
            (formState form' ^. port >= 0)
            PortField
            form'
  where
    sameState = AppState cmdChan (ConnectScreenState s)
    connectState s' = AppState cmdChan (ConnectScreenState s')

-- TODO remove
-- connectScreen :: App ConnectScreenState e VibeMenuName
-- connectScreen =
--   App
--     { appDraw = drawConnectScreen,
--       appHandleEvent = connectScreenHandleEvent,
--       appChooseCursor = focusRingCursor formFocus,
--       appStartEvent = return,
--       appAttrMap = const theMap
--     }

draw :: AppState -> [Widget VibeMenuName]
draw (AppState _cmdChan s) = case s of
  ConnectScreenState form -> drawConnectScreen form
  ConnectingScreenState -> [header "Connecting..."] -- TODO show what we're connecting to
  MainScreenState mss -> drawVibeMenu mss

drawConnectScreen :: Form HostPort e VibeMenuName -> [Widget VibeMenuName]
drawConnectScreen f = [C.vCenter $ C.hCenter form]
  where
    form = B.border $ padTop (Pad 1) $ hLimit 40 $ renderForm f

header = withAttr "header" . txtWrap

title = padBottom (Pad 1) $ header "VibeMenu"

drawVibeMenu :: VibeMenuState -> [Widget VibeMenuName]
drawVibeMenu s = [ui]
  where
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

vibeMenu :: App AppState CustomEvent VibeMenuName
vibeMenu =
  App
    { appDraw = draw,
      appHandleEvent = handleEvent,
      appChooseCursor = chooseCursor,
      appStartEvent = return,
      appAttrMap = const theMap
    }

chooseCursor :: AppState -> [CursorLocation VibeMenuName] -> Maybe (CursorLocation VibeMenuName)
chooseCursor (AppState _cmdChan screenState) ls = case screenState of
  ConnectScreenState css -> focusRingCursor formFocus css ls
  ConnectingScreenState -> neverShowCursor () ls -- TODO
  MainScreenState _mss -> neverShowCursor () ls -- TODO

sendCommand :: BChan Command -> Command -> EventM VibeMenuName ()
sendCommand chan = liftIO . writeBChan chan

connectingStateHandleEvent :: BChan Command -> BrickEvent VibeMenuName CustomEvent -> EventM VibeMenuName (Next AppState)
connectingStateHandleEvent cmdChan = \case
  AppEvent EvConnected -> do
    continue $ AppState cmdChan (MainScreenState initialMainScreenState)
  VtyEvent (V.EvKey (V.KChar 'q') []) -> halt $ AppState cmdChan ConnectingScreenState
  _ -> continue $ AppState cmdChan ConnectingScreenState

handleEvent ::
  AppState ->
  BrickEvent VibeMenuName CustomEvent ->
  EventM VibeMenuName (Next AppState)
handleEvent (AppState cmdChan screenState) ev = case screenState of
  ConnectScreenState form -> connectScreenHandleEvent form cmdChan ev
  -- For now, I don't think ConnectingScreenState should need to send any commands
  ConnectingScreenState -> connectingStateHandleEvent cmdChan ev
  MainScreenState mss -> vibeMenuHandleEvent mss cmdChan ev

-- TODO continueWithoutRedraw where appropriate
vibeMenuHandleEvent ::
  VibeMenuState ->
  BChan Command ->
  BrickEvent VibeMenuName CustomEvent ->
  EventM VibeMenuName (Next AppState)
vibeMenuHandleEvent s cmdChan = \case
  VtyEvent e -> case e of
    V.EvResize {} -> continue $ AppState cmdChan $ MainScreenState s
    V.EvKey V.KEsc [] -> halt $ AppState cmdChan $ MainScreenState s
    V.EvKey (V.KChar c) [] -> case c of
      's' -> do
        sendCommand cmdChan $ CmdButtplug CmdStopAll
        continue $ AppState cmdChan $ MainScreenState s
      'q' -> halt $ AppState cmdChan $ MainScreenState s
      _
        | isDigit c -> do
            withSelectedDevice \(_, Device _ devIndex _) -> do
              let strength =
                    if c == '0'
                      then 1
                      else fromIntegral (ord c - 48) / 10
              sendCommand cmdChan (CmdButtplug $ CmdVibrate devIndex strength)
            continue $ AppState cmdChan $ MainScreenState s
        | otherwise -> continue $ AppState cmdChan $ MainScreenState s
    -- V.EvKey (V.KChar c) []
    --   | '0' <= c && c <= '9' -> do
    --     withSelectedDevice \(_, Device _ devIndex _) -> do
    --        let strength = if c == '0' then 1
    --                                   else fromIntegral (ord c - 48) / 10
    --        sendCommand (s ^. cmdChan) (CmdVibrate devIndex strength)
    --     continue s
    --   | c == 's' -> do
    --     sendCommand (s ^. cmdChan) CmdStopAll
    --     continue s
    --   | otherwise -> continue s
    e -> do
      s' <- handleEventLensed s devices L.handleListEvent e
      continue $ AppState cmdChan $ MainScreenState s'
  AppEvent e -> case e of
    ReceivedMessage msg ->
      continue $
        AppState cmdChan $
          MainScreenState $
            s
              & messageLog
                %~ (listAppend msg .> L.listMoveToEnd)
    ReceivedDeviceList devs ->
      continue $ AppState cmdChan $ MainScreenState $ s & devices .~ L.list DeviceMenu (Vec.fromList devs) 1
    EvDeviceAdded dev@(Device devName (fromIntegral -> ix) _) ->
      continue $ AppState cmdChan $ MainScreenState $ s & devices %~ listAppend dev
    EvDeviceRemoved (fromIntegral -> ix) ->
      continue $ AppState cmdChan $ MainScreenState $ s & devices %~ deleteDeviceByIndex ix
  _ -> continue $ AppState cmdChan $ MainScreenState s
  where
    selectedDevice = s ^. devices & L.listSelectedElement

    withSelectedDevice ::
      ((Int, Device) -> EventM VibeMenuName ()) ->
      EventM VibeMenuName ()
    withSelectedDevice k = case selectedDevice of
      Just d -> k d
      Nothing -> pure ()

listAppend :: e -> L.List n e -> L.List n e
listAppend elt l =
  let n = Vec.length $ L.listElements l
   in L.listInsert n elt l

deleteDeviceByIndex :: Word -> L.List n Device -> L.List n Device
deleteDeviceByIndex devIx l =
  case Vec.findIndex ((devIx ==) . deviceIndex) (L.listElements l) of
    Just listIndex -> L.listRemove listIndex l
    Nothing -> l

-- getHostPort :: IO (String, Int, V.Vty)
-- getHostPort = do
--   args <- getArgs
--   initialVty <- buildVty
--   case args of
--     [host, port] -> pure (host, read port, initialVty)
--     _ -> do
--       let initialHost = "127.0.0.1"
--           connectForm = setFieldValid True PortField $ mkForm initialHostPort
--           initialHostPort = HostPort initialHost 12345

--       (connectForm', vty') <-
--         customMainWithVty
--           initialVty
--           buildVty
--           Nothing
--           connectScreen
--           connectForm
--       if allFieldsValid connectForm'
--         then do
--           putStrLn "The final form inputs were valid."
--           let HostPort host port = formState connectForm'
--           pure (T.unpack host, port, vty')
--         else do
--           putStrLn $
--             "The final form had invalid inputs: "
--               <> show (invalidFields connectForm')
--           exitFailure

buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

initialMainScreenState :: VibeMenuState
initialMainScreenState =
  VibeMenuState
    (L.list MessageLog mempty 1)
    (L.list DeviceMenu mempty 1)

main :: IO ()
main = do
  vty <- buildVty
  args <- getArgs
  let mConnector = case args of
        [host, port] -> Just $ BPWS.Connector host (read port)
        _ -> Nothing

  cmdChan <- newBChan 30

  let defaultHostPort = HostPort "127.0.0.1" 12345
      connectForm = setFieldValid True PortField $ mkForm defaultHostPort
      initialState = AppState cmdChan $ case mConnector of
        Just connector -> ConnectingScreenState
        Nothing -> ConnectScreenState connectForm

      startEvent = case mConnector of
        Just connector -> \s -> do
          sendCommand cmdChan (CmdConnect connector)
          pure s
        Nothing -> pure

  evChan <- newBChan 10 -- tweak chan capacity
  race_
    (handleCommands evChan cmdChan)
    (customMain vty buildVty (Just evChan) vibeMenu initialState)
  pure ()

handleCommands :: BChan CustomEvent -> BChan Command -> IO ()
handleCommands evChan cmdChan = do
  -- used for forwarding buttplug specific commands to the thread connected to
  -- the buttplug server
  buttplugCmdsChan :: BChan ButtplugCommand <- newBChan 5
  connected <- newEmptyMVar

  let handleCommand :: Command -> IO ()
      handleCommand command = do
        hPutStrLn stderr $ "DEBUG: got command: " <> show command
        case command of
          CmdConnect connector -> connect connector
          CmdButtplug cmd -> do
            tryTakeMVar connected >>= \case
              -- we need a way to forward some UI commands to the thread thats
              -- connected to buttplug, if it exists
              Just () -> writeBChan buttplugCmdsChan cmd
              -- we shouldn't be getting ButtplugCommands when we're not connected
              Nothing -> error "received ButtplugCommand from ui when not connected to the buttplug server!"

      connect :: BPWS.Connector -> IO ()
      connect connector = do
        putStrLn $
          "Connecting to: "
            ++ connector.wsConnectorHost
            ++ ":"
            ++ show connector.wsConnectorPort
        BPWS.runClient connector \handle -> do
          putStrLn "connected!"
          putMVar connected ()
          writeBChan evChan EvConnected
          handleMsgs handle evChan buttplugCmdsChan
          pure ()
        takeMVar connected

  -- TODO handle in parallel
  S.mapM_ handleCommand $
    S.fromParallel $
      S.repeatM (readBChan cmdChan)

handleMsgs ::
  Buttplug.Handle ->
  BChan CustomEvent ->
  BChan ButtplugCommand ->
  IO ()
handleMsgs handle evChan buttplugCmdsChan = do
  -- block while we perform handshake
  Buttplug.sendMessage handle $ MsgRequestServerInfo 1 "VibeMenu" clientMessageVersion
  [servInfo@(MsgServerInfo 1 _ _ _)] <- Buttplug.receiveMessages handle
  writeBChan evChan $ ReceivedMessage servInfo

  mapConcurrently_
    id
    [ Buttplug.sendMessage handle $ MsgRequestDeviceList 2,
      Buttplug.sendMessage handle $ MsgStartScanning 3,
      -- main loop
      buttplugMessages handle
        |> S.concatMap (toEvents .> S.fromFoldable)
        |> S.mapM_ (writeBChan evChan),
      S.mapM_ handleButtplugCmd $
        S.repeatM $
          readBChan buttplugCmdsChan
    ]
  where
    handleButtplugCmd :: ButtplugCommand -> IO ()
    handleButtplugCmd = \case
      CmdStopAll -> Buttplug.sendMessage handle $ MsgStopAllDevices 1
      CmdVibrate devIx speed ->
        Buttplug.sendMessage handle $
          MsgVibrateCmd 1 devIx [Vibrate 0 speed]

uiCmds :: (IsStream t) => BChan Command -> t IO Command
uiCmds chan = S.repeatM (readBChan chan)

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
toEvents :: Message -> [CustomEvent]
toEvents msg =
  catMaybes
    [ Just $ ReceivedMessage msg,
      msgToCustomEvent msg
    ]
  where
    -- Translate messages that the UI needs to know about to events, discarding
    -- the unnecessary ones
    -- TODO this function is mostly just for ignoring message indices. This should reside in
    -- a high level buttplug library. Users of buttplug shouldn't have to worry about message
    -- indices
    msgToCustomEvent :: Message -> Maybe CustomEvent
    msgToCustomEvent = \case
      MsgDeviceAdded _ name ix devmsgs -> Just $ EvDeviceAdded $ Device name ix devmsgs
      MsgDeviceRemoved _ ix -> Just $ EvDeviceRemoved ix
      MsgDeviceList _ devices -> Just $ ReceivedDeviceList devices
      _ -> Nothing
