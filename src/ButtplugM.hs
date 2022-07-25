{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ButtplugM where

-- start with a concrete monad, we'll figure out how to do a transformer later

import Buttplug.Core.Handle (Handle)
import Buttplug.Core.Handle qualified as Handle
import Buttplug.Core.Message qualified as CoreMsg
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Message

-- TODO
-- Eventually we'll automatically handle giving messages correct message ids.
-- We'll present users with an api where they don't need to worry about id
-- We should also maintain a list of available devices, and allow users to request them directly

newtype ButtplugM a = ButtplugM {runButtplugM :: ReaderT (Handle, TVar Word) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadThrow,
      MonadIO,
      MonadReader (Handle, TVar Word),
      MonadUnliftIO,
      MonadBase IO
    )

instance MonadBaseControl IO ButtplugM where
  type StM ButtplugM a = a
  liftBaseWith f = ButtplugM $ liftBaseWith $ \q -> f (q . runButtplugM)
  restoreM = ButtplugM . restoreM

runButtplug :: Handle -> ButtplugM a -> IO a
runButtplug handle bp = do
  msgIndexCounter <- newTVarIO 1
  flip runReaderT (handle, msgIndexCounter) . runButtplugM $ bp

sendCoreMessages :: [CoreMsg.Message] -> ButtplugM ()
sendCoreMessages msgs = withHandle $ \h -> Handle.sendMessages h msgs

sendCoreMessage :: CoreMsg.Message -> ButtplugM ()
sendCoreMessage msg = withHandle $ \h -> Handle.sendMessage h msg

sendMessages :: [Message] -> ButtplugM ()
sendMessages msgs = do
  msgIdVar <- getCurrMsgId
  msgId <- liftIO $
    atomically $
      stateTVar msgIdVar $
        \n -> (n, n + fromIntegral (length msgs))
  let coreMsgs = zipWith withMsgId [msgId ..] msgs
  sendCoreMessages coreMsgs

sendMessage :: Message -> ButtplugM ()
sendMessage msg = do
  msgIdVar <- getCurrMsgId
  msgId <- liftIO $ atomically $ stateTVar msgIdVar $ \n -> (n, n + 1)
  sendCoreMessage $ withMsgId msgId msg

receiveCoreMessages :: ButtplugM [CoreMsg.Message]
receiveCoreMessages = withHandle Handle.receiveMessages

receiveMessages :: ButtplugM [Message]
receiveMessages = map withoutMsgId <$> receiveCoreMessages

getHandle :: ButtplugM Handle
getHandle = fst <$> ask

getCurrMsgId :: ButtplugM (TVar Word)
getCurrMsgId = snd <$> ask

withHandle :: (Handle -> IO a) -> ButtplugM a
withHandle k = getHandle >>= liftIO . k

handshake :: ButtplugM Message
handshake = do
  sendMessage $ MsgRequestServerInfo "VibeMenu" clientMessageVersion
  [servInfo@(MsgServerInfo {})] <- receiveMessages
  pure servInfo

