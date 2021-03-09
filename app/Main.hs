{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where


import qualified Data.Text as T
import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Foldable (traverse_)
import System.Environment
import System.Exit (exitFailure)
import System.Process

import           Data.Maybe (catMaybes)
import qualified Data.Vector as Vec
import           Data.Vector (Vector)
import           Data.Char (ord)

import qualified Graphics.Vty as V
import Brick
import Brick.Forms ( Form
                   , newForm
                   , formState
                   , formFocus
                   , setFieldValid
                   , renderForm
                   , handleFormEvent
                   , invalidFields
                   , allFieldsValid
                   , focusedFormInputAttr
                   , invalidFormInputAttr
                   , checkboxField
                   , radioField
                   , editShowableField
                   , editTextField
                   , editPasswordField
                   , (@@=)
                   )
import Brick.Focus ( focusGetCurrent
                   , focusRingCursor
                   )
import Brick.BChan ( newBChan
                   , writeBChan
                   , readBChan
                   , BChan
                   )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A

import Streamly hiding ((<=>))
import Streamly.Prelude ((|:), nil)
import qualified Streamly.Prelude as S

import Flow

import Buttplug.Core

data ConnectScreenName = HostField
                       | PortField
          deriving (Eq, Ord, Show)

data HostPort = HostPort { _host :: T.Text, _port :: Int }
  deriving (Show, Eq)

---------------------------

makeLenses ''HostPort

-- This form is covered in the Brick User Guide; see the "Input Forms"
-- section.
mkForm :: HostPort -> Form HostPort e ConnectScreenName
mkForm =
    let label s w = padBottom (Pad 1) $
                    vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [ label "Host" @@=
                   editTextField host HostField (Just 1)
               , label "Port" @@=
                   editShowableField port PortField
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , ("header", V.black `on` V.white)
  , (L.listAttr,            V.white `on` V.black)
  , (L.listSelectedAttr,    V.white `on` V.blue)
  ]

drawConnectScreen :: Form HostPort e ConnectScreenName -> [Widget ConnectScreenName]
drawConnectScreen f = [C.vCenter $ C.hCenter form]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 40 $ renderForm f

connectScreen :: App (Form HostPort e ConnectScreenName) e ConnectScreenName
connectScreen =
    App { appDraw = drawConnectScreen
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent V.EvResize {}     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey V.KEnter []) -> halt s
                _ -> do
                    s' <- handleFormEvent ev s

                    -- Example of external validation:
                    continue $ setFieldValid
                      (formState s' ^. port >= 0) PortField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

data VibeMenuName = MessageLog
                  | DeviceMenu
  deriving (Eq, Ord, Show)

data Command = CmdStopAll
             | CmdVibrate Word Double


data VibeMenuState =
  VibeMenuState { _messageLog :: L.List VibeMenuName Message
                , _devices :: L.List VibeMenuName Device
                , _cmdChan :: BChan Command
                }

makeLenses ''VibeMenuState


data CustomEvent = ReceivedMessage Message
                 | ReceivedDeviceList [Device]
                 | EvDeviceAdded Device
                 | EvDeviceRemoved Word

drawVibeMenu s = [ ui ]
  where header = withAttr "header" . txtWrap
        title = padBottom (Pad 1) $ header "VibeMenu"
        deviceMenu =
          header "Connected Devices"
          <=>
          padBottom (Pad 1) ( L.renderList listDrawDevice True $ s ^. devices)
        receivedMsgLog =
          header "Message log"
          <=>
          padBottom (Pad 1) (L.renderList listDrawElement False $ s ^. messageLog)

        ui = title
             <=>
             deviceMenu
             <=>
             receivedMsgLog

listDrawDevice :: Bool -> Device -> Widget VibeMenuName
listDrawDevice sel Device {..}
  | sel       = withAttr L.listSelectedAttr $ txt label
  | otherwise =                               txt label
  where label :: T.Text
        label = T.pack (show deviceIndex) <> " " <> deviceName

listDrawElement :: (Show e) => Bool -> e -> Widget VibeMenuName
listDrawElement sel a =
     let selStr s = if sel
                    then withAttr L.listSelectedAttr $ str s 
                    else str s
     in (selStr $ show a)

vibeMenu :: App VibeMenuState CustomEvent VibeMenuName
vibeMenu =
    App { appDraw = drawVibeMenu
        , appHandleEvent = vibeMenuHandleEvent
        , appChooseCursor = neverShowCursor -- TODO
        , appStartEvent = return
        , appAttrMap = const theMap
        }

sendCommand :: BChan Command -> Command -> EventM n ()
sendCommand chan = liftIO . writeBChan chan

vibeMenuHandleEvent :: VibeMenuState
                    -> BrickEvent VibeMenuName CustomEvent
                    -> EventM VibeMenuName (Next VibeMenuState)
vibeMenuHandleEvent s = \case
  VtyEvent e -> case e of
    V.EvResize {} -> continue s
    V.EvKey V.KEsc [] -> halt s
    V.EvKey (V.KChar c) [] 
      | '0' <= c && c <= '9' -> do
        withSelectedDevice \(_, Device _ devIndex _) -> do
           let strength = if c == '0' then 1
                                      else fromIntegral (ord c - 48) / 10
           sendCommand (s ^. cmdChan) (CmdVibrate devIndex strength)
        continue s
      | c == 's' -> do 
        withSelectedDevice \(_, Device _ devIndex _) ->
          sendCommand (s ^. cmdChan) CmdStopAll
        continue s
      | otherwise -> continue s
    e -> handleEventLensed s devices L.handleListEvent e >>= continue
  AppEvent e -> case e of
    ReceivedMessage msg -> continue $ s & messageLog %~ listAppend msg
    ReceivedDeviceList devs ->
      continue $ s & devices .~ L.list DeviceMenu (Vec.fromList devs) 1
    EvDeviceAdded dev@(Device devName (fromIntegral -> ix) _) ->
      continue $ s & devices %~ listAppend dev
    EvDeviceRemoved (fromIntegral -> ix) ->
      continue $ s & devices %~ deleteDeviceByIndex ix
  _ -> continue s
  where selectedDevice = s ^. devices & L.listSelectedElement

        withSelectedDevice :: ((Int, Device) -> EventM VibeMenuName ())
                           -> EventM VibeMenuName ()
        withSelectedDevice k = case selectedDevice of
          Just d  -> k d
          Nothing -> pure ()

listAppend :: e -> L.List n e -> L.List n e
listAppend elt l = let n = Vec.length $ L.listElements l
                    in L.listInsert n elt l

deleteDeviceByIndex :: Word -> L.List n Device -> L.List n Device
deleteDeviceByIndex devIx l =
  case Vec.findIndex ((devIx==) . deviceIndex) (L.listElements l) of
    Just listIndex -> L.listRemove listIndex l
    Nothing -> l


getHostPort :: IO (String, Int, V.Vty)
getHostPort = do
    args <- getArgs
    initialVty <- buildVty
    case args of
      [host, port] -> pure (host, read port, initialVty)
      _      -> do

        initialHost <-
          (<> ".local") . T.strip . T.pack <$> readProcess "hostname" [] ""
        let connectForm = setFieldValid True PortField $
              mkForm initialHostPort
            initialHostPort = HostPort initialHost 12345

        (connectForm', vty') <- customMainWithVty
          initialVty buildVty Nothing connectScreen connectForm
        if allFieldsValid connectForm'
           then do
             putStrLn "The final form inputs were valid."
             let HostPort host port = formState connectForm' 
             pure (T.unpack host, port, vty')
           else do 
             putStrLn $ "The final form had invalid inputs: " <>
               show (invalidFields connectForm')
             exitFailure

buildVty = do
      v <- V.mkVty =<< V.standardIOConfig
      V.setMode (V.outputIface v) V.Mouse True
      return v

main :: IO ()
main = do

    (host, port, vty) <- getHostPort
    cmdChan <- newBChan 30

    let connector = InsecureWebSocketConnector host port
        initialState = VibeMenuState (L.list MessageLog mempty 1)
                                     (L.list DeviceMenu mempty 1)
                                     cmdChan


    putStrLn $ "Connecting to: " <> host <> ":" <> show port
    runClient connector \con -> do
      putStrLn "connected!"
      evChan <- newBChan 10 -- tweak chan capacity
      race_
        (handleMsgs con evChan cmdChan)
        (customMain vty buildVty (Just evChan) vibeMenu initialState)
      pure ()


handleMsgs :: Connection WebSocketConnector 
           -> BChan CustomEvent
           -> BChan Command
           -> IO ()
handleMsgs con evChan cmdChan = do
  -- block while we perform handshake
  sendMessage con $ RequestServerInfo 1 "VibeMenu" clientMessageVersion
  [servInfo@(ServerInfo 1 _ _ _)] <- receiveMsgs con
  writeBChan evChan $ ReceivedMessage servInfo

  doConcurrently_
    [ sendMessage con $ RequestDeviceList 2
    , sendMessage con $ StartScanning 3
    -- main loop
    , buttplugMessages con
      |> S.concatMap (toEvents .> S.fromFoldable)
      |> S.mapM_ (writeBChan evChan)
    , uiCmds cmdChan
      |> S.mapM_ (handleCmd con)
    ]

handleCmd :: Connector c => Connection c -> Command -> IO ()
handleCmd con = \case
  CmdStopAll             -> sendMessage con $ StopAllDevices 1
  CmdVibrate devIx speed -> sendMessage con $
    VibrateCmd 1 devIx [Vibrate 0 speed]

doConcurrently_ = mapConcurrently_ id

uiCmds :: (IsStream t) => BChan Command -> t IO Command
uiCmds chan = S.repeatM (readBChan chan)

-- Produces all messages that come in through a buttplug connection
buttplugMessages :: (IsStream t, Connector c)
                 => Connection c -> t IO Message
--buttplugMessages con = forever $ lift (receiveMsgs con) >>= each
buttplugMessages con = S.repeatM (receiveMsgs con)
                     & S.concatMap S.fromFoldable


-- We notify the UI of every message so it can display them, but also
-- translate messages into simplified events
toEvents :: Message -> [CustomEvent]
toEvents msg = catMaybes
  [ Just $ ReceivedMessage msg
  , msgToCustomEvent msg
  ]
  where
  -- Translate messages that the UI needs to know about to events, discarding 
  -- the unnecessary ones
  msgToCustomEvent :: Message -> Maybe CustomEvent
  msgToCustomEvent = \case
    DeviceAdded _ name ix devmsgs -> Just $ EvDeviceAdded $ Device name ix devmsgs
    DeviceRemoved _ ix            -> Just $ EvDeviceRemoved ix
    DeviceList _ devices          -> Just $ ReceivedDeviceList devices
    _ -> Nothing
