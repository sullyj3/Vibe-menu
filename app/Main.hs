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

import qualified Data.Vector as Vec
import           Data.Vector (Vector)

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

import Pipes
import qualified Pipes.Prelude as P

import Buttplug.Core
import ButtPipe

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
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
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
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey V.KEnter []) -> halt s
                _ -> do
                    s' <- handleFormEvent ev s

                    -- Example of external validation:
                    continue $ setFieldValid
                      ((formState s')^.port >= 0) PortField s'

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

data VibeMenuName = MessageLog
                  | DeviceMenu
  deriving (Eq, Ord, Show)

data VibeMenuState =
  VibeMenuState { _messageLog :: L.List VibeMenuName Message
                , _devices :: L.List VibeMenuName Device
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
          (header "Connected Devices")
          <=>
          (padBottom (Pad 1) $ (L.renderList listDrawDevice True $ s ^. devices))
        receivedMsgLog =
          (header "Message log")
          <=>
          (padBottom (Pad 1) $ (L.renderList listDrawElement False $ s ^. messageLog))

        ui = title
             <=>
             deviceMenu
             <=>
             receivedMsgLog

listDrawDevice :: Bool -> Device -> Widget VibeMenuName
listDrawDevice sel (Device {..})
  | sel       = withAttr L.listSelectedAttr $ txt label
  | otherwise =                               txt label
  where label :: T.Text
        label = (T.pack $ show deviceIndex) <> " " <> deviceName

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

vibeMenuHandleEvent :: VibeMenuState
                    -> BrickEvent VibeMenuName CustomEvent
                    -> EventM VibeMenuName (Next VibeMenuState)
vibeMenuHandleEvent s = \case
  VtyEvent (V.EvResize {})     -> continue s
  VtyEvent (V.EvKey V.KEsc [])   -> halt s
  VtyEvent (V.EvKey V.KEnter []) -> continue s -- TODO
  VtyEvent e -> handleEventLensed s messageLog L.handleListEvent e
    >>= continue
  AppEvent (ReceivedMessage msg) -> do
    let len = s ^. messageLog & length
    continue $ s & messageLog %~ (L.listInsert len msg)
  AppEvent (ReceivedDeviceList devs) -> do
    continue $ s & devices .~ L.list DeviceMenu (Vec.fromList devs) 1

  AppEvent (EvDeviceAdded dev@(Device devName (fromIntegral -> ix) _)) -> do
      continue $ s & devices %~ listAppend dev
  AppEvent (EvDeviceRemoved (fromIntegral -> ix)) -> do
      continue $ s & devices %~ deleteDeviceByIndex ix
  _ -> continue s

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

    let connector = InsecureWebSocketConnector host port
        initialState = VibeMenuState (L.list MessageLog mempty 1)
                                     (L.list DeviceMenu mempty 1)


    putStrLn $ "Connecting to: " <> host <> ":" <> show port
    runClient connector \con -> do
      putStrLn "connected!"
      evChan <- newBChan 10 -- tweak chan capacity
      race_
        (handleMsgs con evChan)
        (customMain vty buildVty (Just evChan) vibeMenu initialState)
      pure ()


handleMsgs :: Connection WebSocketConnector -> BChan CustomEvent -> IO ()
handleMsgs con evChan = do
  -- initial setup - handshake, get connected devices, and start scanning
  sendMessage con $ RequestServerInfo 1 "VibeMenu" 2
  [servInfo@(ServerInfo 1 _ _ _)] <- receiveMsgs con
  writeBChan evChan $ ReceivedMessage servInfo

  sendMessage con $ RequestDeviceList 2
  [devList@(DeviceList 2 devices)] <- receiveMsgs con
  writeBChan evChan $ ReceivedMessage devList
  writeBChan evChan $ ReceivedDeviceList devices

  sendMessage con $ StartScanning 3

  -- main loop
  runEffect $ buttplugMessages con >-> toEvents >-> P.mapM_ (writeBChan evChan)


-- Produces all messages that come in through a buttplug connection
buttplugMessages :: Connector c => Connection c -> Producer Message IO ()
buttplugMessages con = forever $ do
  msgs <- lift $ receiveMsgs con
  mapM_ yield msgs


-- We notify the UI of every message so it can display them, but also
-- translate messages into simplified events
toEvents :: Pipe Message CustomEvent IO ()
toEvents = forever do
  msg <- await
  yield (ReceivedMessage msg)
  case msgToCustomEvent msg of
    Just ev -> yield ev
    Nothing -> pure ()


-- Translate messages that the UI needs to know about to events, discarding 
-- others
msgToCustomEvent :: Message -> Maybe CustomEvent
msgToCustomEvent = \case
  DeviceAdded _ name ix devmsgs -> Just $ EvDeviceAdded $ Device name ix devmsgs
  DeviceRemoved _ ix -> Just $ EvDeviceRemoved ix
  DeviceList _ devices -> Just $ ReceivedDeviceList devices
  _ -> Nothing

