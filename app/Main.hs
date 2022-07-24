{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Brick
import Brick.BChan
  ( BChan,
    newBChan,
    readBChan,
    writeBChan,
  )
import Buttplug.Core (Device (..), Message (..), Vibrate (..), clientMessageVersion)
import Buttplug.Core.Handle qualified as Buttplug
import Buttplug.Core.WebSockets qualified as BPWS
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Graphics.Vty qualified as V
import HandleBrickEvent
import Ki
import Streamly.Data.Fold qualified as F
import Streamly.Prelude (IsStream)
import Streamly.Prelude qualified as S
import System.Environment
import System.IO
import Types
import View

vibeMenu ::
  (AppState -> EventM VibeMenuName AppState) ->
  App AppState VibeMenuEvent VibeMenuName
vibeMenu startEvent =
  App
    { appDraw = drawVibeMenu,
      appHandleEvent = vibeMenuHandleEvent,
      appChooseCursor = neverShowCursor, -- TODO
      appStartEvent = startEvent,
      appAttrMap = const theMap
    }

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

  vty <- buildVty

  let (initialState, startEvent) = case mConnector of
        Just connector ->
          let sendCmdConnect s = do
                liftIO $ writeBChan cmdChan' (CmdConnect connector)
                pure s
           in (AppState cmdChan' ConnectingScreen, sendCmdConnect)
        Nothing -> (AppState cmdChan' (ConnectScreen $ mkConnectForm defaultHostPort), pure)

  evChan :: BChan VibeMenuEvent <- newBChan 10
  _ <- scoped \scope -> do
    _ <- fork scope $ workerThread cmdChan' evChan
    uiThread <-
      fork scope $
        customMain vty buildVty (Just evChan) (vibeMenu startEvent) initialState
    atomically $ await uiThread
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
  BPWS.runClient connector \handle -> do
    writeBChan evChan EvConnected
    sendReceiveBPMessages handle evChan buttplugCmdChan

sendReceiveBPMessages ::
  Buttplug.Handle ->
  BChan VibeMenuEvent ->
  BChan ButtplugCommand ->
  IO ()
sendReceiveBPMessages handle evChan buttplugCmdChan = do
  servInfo <- handShake
  emitBPEvent $ ReceivedMessage servInfo
  scoped \scope -> do
    _ <- fork scope $ Buttplug.sendMessage handle $ MsgRequestDeviceList 2
    _ <- fork scope $ Buttplug.sendMessage handle $ MsgStartScanning 3
    _ <- fork scope emitEvents
    _ <- fork scope handleCmds
    atomically $ awaitAll scope
  where
    handShake = do
      Buttplug.sendMessage handle $ MsgRequestServerInfo 1 "VibeMenu" clientMessageVersion
      [servInfo@(MsgServerInfo 1 _ _ _)] <- Buttplug.receiveMessages handle
      pure servInfo
    emitBPEvent = writeBChan evChan . BPEvent
    emitEvents =
      S.mapM_ emitBPEvent $
        S.concatMap (S.fromFoldable . toEvents) $
          S.tap logErrors $
            buttplugMessages handle
    handleCmds =
      S.mapM_ (handleButtplugCommand handle) $
        (S.repeatM . readBChan) buttplugCmdChan

    logErrors :: F.Fold IO Message ()
    logErrors = F.drainBy \case
      MsgError _ msg code -> T.hPutStrLn stderr $ displayBPErrorMsg msg code
      _ -> pure ()

    -- TODO might be useful to have this in buttplug-hs-core
    displayBPErrorMsg msg code = "Buttplug error " <> T.pack (show code) <> ": " <> msg

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
      MsgDeviceList _ deviceList -> Just $ ReceivedDeviceList deviceList
      _ -> Nothing
