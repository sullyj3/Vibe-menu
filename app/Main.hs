{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH
import Control.Monad (forever)
import Data.Foldable (traverse_)
import System.Environment
import System.Exit (exitFailure)
import System.Process

import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( Form
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
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import Brick.BChan
  ( newBChan
  , writeBChan
  , readBChan
  , BChan
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A

import Buttplug.Core
import Control.Concurrent.Async

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

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , ("header", V.black `on` V.white)
  , (L.listAttr,            V.white `on` V.black)
  , (L.listSelectedAttr,    V.blue `on` V.black)
  , (customAttr,            fg V.cyan)
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
  deriving (Eq, Ord, Show)

data VibeMenuState =
  VMSt { _messageLog :: L.List VibeMenuName Message -- TODO replace with list for scrolling
       }

makeLenses ''VibeMenuState


data CustomEvent = ReceivedMessage Message

drawVibeMenu s = [ ui ]
  where title = padBottom (Pad 1) $ withAttr "header" $ txtWrap "VibeMenu"
        receivedMsgLog =
          (withAttr "header" $ txtWrap "Message log")
          <=>
          (padBottom (Pad 1) $ (L.renderList listDrawElement True $ s ^. messageLog))
        ui = title
             <=>
             receivedMsgLog

listDrawElement :: (Show e) => Bool -> e -> Widget VibeMenuName
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in (selStr $ show a)

vibeMenu :: App VibeMenuState CustomEvent VibeMenuName
vibeMenu =
    App { appDraw = drawVibeMenu
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                VtyEvent (V.EvKey V.KEnter []) -> continue s -- TODO
                VtyEvent e -> do
                  l' <- L.handleListEvent e (s ^. messageLog)
                  continue (s & messageLog .~ l')
                AppEvent (ReceivedMessage msg) -> do
                  let len = s ^. messageLog & length
                  continue $ s & messageLog %~ (L.listInsert len msg)
                _ -> continue s

        , appChooseCursor = neverShowCursor -- TODO
        , appStartEvent = return
        , appAttrMap = const theMap
        }

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

    let --HostPort host port = formState connectForm'
        connector = InsecureWebSocketConnector host port
        initialState = VMSt (L.list MessageLog mempty 1)


    putStrLn $ "Connecting to: " <> host <> ":" <> show port
    runClient connector \con -> do
      putStrLn "connected!"
      msgChan <- newBChan 10 -- tweak chan capacity
      race_
        (handleMsgs con msgChan)
        (customMain vty buildVty (Just msgChan) vibeMenu initialState)
      pure ()


handleMsgs :: Connection WebSocketConnector -> BChan CustomEvent -> IO ()
handleMsgs con msgChan = do
  sendMessage con $ RequestServerInfo 1 "VibeMenu" 2
  [ServerInfo 1 _ _ _] <- receiveMsgs con
  sendMessage con $ StartScanning 2
  (forever do msgs <- receiveMsgs con
              traverse_ (writeBChan msgChan . ReceivedMessage) msgs)
