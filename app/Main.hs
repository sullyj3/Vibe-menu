{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Brick (App (..), EventM, customMain, neverShowCursor)
import Brick.BChan
  ( BChan,
    newBChan,
    readBChan,
    writeBChan,
  )
import Buttplug.ButtplugM (ButtplugM)
import Buttplug.ButtplugM qualified as ButtplugM
import Buttplug.Device (Device (..))
import Buttplug.Handle qualified as Buttplug
import Buttplug.WebSockets qualified as BPWS
import Buttplug.Message
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class (lift)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Graphics.Vty qualified as V
import HandleBrickEvent (vibeMenuHandleEvent)
import Ki.Unlifted (await, awaitAll, fork, scoped)
import Lens.Micro ((^.))
import Streamly.Data.Fold qualified as F
import Streamly.Prelude (IsStream)
import Streamly.Prelude qualified as S
import System.Environment (getArgs)
import System.IO (stderr)
import Types
  ( AppState (AppState),
    BPSessionEvent (..),
    ButtplugCommand (..),
    Command (..),
    HostPort (HostPort),
    ScreenState (ConnectScreen, ConnectingScreen),
    VibeMenuEvent (..),
    VibeMenuName,
    appCmdChan,
    mkConnectForm,
  )
import View (drawVibeMenu, theMap)

vibeMenu ::
  Maybe BPWS.Connector ->
  App AppState VibeMenuEvent VibeMenuName
vibeMenu mConnector =
  App
    { appDraw = drawVibeMenu,
      appHandleEvent = vibeMenuHandleEvent,
      appChooseCursor = neverShowCursor, -- TODO
      appStartEvent = startEvent,
      appAttrMap = const theMap
    }
  where
    startEvent :: AppState -> EventM VibeMenuName AppState
    startEvent = case mConnector of
      Just connector -> \s -> do
        liftIO $ writeBChan (s ^. appCmdChan) (CmdConnect connector)
        pure s
      Nothing -> pure

-- TODO switch to optparse applicative maybe
parseArgs :: IO (Maybe BPWS.Connector)
parseArgs = do
  args <- getArgs
  pure $ case args of
    [h, p] -> Just $ BPWS.Connector h (read p)
    [p] -> Just $ BPWS.Connector defaultHost (read p)
    _ -> Nothing

defaultHost :: IsString s => s
defaultHost = "127.0.0.1"

defaultPort :: Int
defaultPort = 12345

defaultHostPort :: HostPort
defaultHostPort = HostPort defaultHost defaultPort

buildVty :: IO V.Vty
buildVty = do
  v <- V.mkVty =<< V.standardIOConfig
  V.setMode (V.outputIface v) V.Mouse True
  return v

-- todo: proper logging with simple-logger
-- https://hackage.haskell.org/package/simple-logger-0.1.1/docs/Control-Logger-Simple.html
main :: IO ()
main = do
  mConnector <- parseArgs
  cmdChan' <- newBChan 30
  let initialState = case mConnector of
        Just connector -> AppState cmdChan' (ConnectingScreen connector)
        Nothing -> AppState cmdChan' (ConnectScreen $ mkConnectForm defaultHostPort)

  evChan <- newBChan 10
  scoped \scope -> do
    _ <- fork scope $ workerThread cmdChan' evChan
    vty <- buildVty
    customMain vty buildVty (Just evChan) (vibeMenu mConnector) initialState
  pure ()

-- Main background thread
workerThread :: BChan Command -> BChan VibeMenuEvent -> IO ()
workerThread cmdChan evChan = do
  buttplugCmdChan <- newBChan 30
  scoped \scope -> do
    forever $
      readBChan cmdChan >>= \case
        CmdConnect connector -> do
          _ <- fork scope $ connect connector buttplugCmdChan evChan
          pure ()
        BPCommand bpCmd -> writeBChan buttplugCmdChan bpCmd

-- Background thread which handles communication with the server
connect :: BPWS.Connector -> BChan ButtplugCommand -> BChan VibeMenuEvent -> IO ()
connect connector buttplugCmdChan evChan = do
  putStrLn $
    "Connecting to: " <> connector.wsConnectorHost <> ":" <> show connector.wsConnectorPort
  -- TODO handle exceptions
  BPWS.runButtplugWebSockets connector do
    liftIO $ writeBChan evChan EvConnected
    sendReceiveBPMessages evChan buttplugCmdChan

sendReceiveBPMessages ::
  BChan VibeMenuEvent ->
  BChan ButtplugCommand ->
  ButtplugM ()
sendReceiveBPMessages evChan buttplugCmdChan = do
  servInfo <- ButtplugM.handshake
  emitBPEvent $ ReceivedMessage servInfo
  ButtplugM.sendMessages [MsgRequestDeviceList, MsgStartScanning]
  scoped \scope -> do
    -- Send events to the Brick UI
    _ <- fork scope emitEvents
    -- receive commands from the Brick UI
    handleCmds
  where
    emitBPEvent = liftIO . writeBChan evChan . BPEvent

    emitEvents :: ButtplugM ()
    emitEvents =
      S.mapM_ emitBPEvent $
        S.concatMap (S.fromFoldable . toEvents) $
          S.trace (liftIO . logErrors) buttplugMessages

    handleCmds :: ButtplugM ()
    handleCmds =
      S.mapM_ handleButtplugCommand $
        (S.repeatM . liftIO . readBChan) buttplugCmdChan

    logErrors :: Message -> IO ()
    logErrors = \case
      MsgError msg code -> T.hPutStrLn stderr $ displayBPErrorMsg msg code
      _ -> pure ()

    -- TODO might be useful to have this in buttplug-hs-core
    displayBPErrorMsg msg code = "Buttplug error " <> T.pack (show code) <> ": " <> msg

-- forward messages from the UI to the buttplug server
handleButtplugCommand :: ButtplugCommand -> ButtplugM ()
handleButtplugCommand = \case
  CmdStopAll -> ButtplugM.sendMessage $ MsgStopAllDevices
  CmdVibrate devIx speed ->
    ButtplugM.sendMessage $
      MsgVibrateCmd devIx [Vibrate 0 speed]

-- Produces all messages that come in through a buttplug connection
buttplugMessages :: IsStream t => t ButtplugM Message
buttplugMessages =
  S.concatMap S.fromFoldable $
    S.repeatM ButtplugM.receiveMessages

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
      MsgDeviceAdded name ix devmsgs -> Just $ EvDeviceAdded $ Device name ix devmsgs
      MsgDeviceRemoved ix -> Just $ EvDeviceRemoved ix
      MsgDeviceList deviceList -> Just $ ReceivedDeviceList deviceList
      _ -> Nothing
