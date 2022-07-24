{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ButtplugM where

-- start with a concrete monad, we'll figure out how to do a transformer later

import Buttplug.Core (Message)
import Buttplug.Core.Handle (Handle)
import Buttplug.Core.Handle qualified as Handle
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

-- TODO
-- Eventually we'll automatically handle giving messages correct message ids.
-- We'll present users with an api where they don't need to worry about id
-- We should also maintain a list of available devices, and allow users to request them directly

newtype ButtplugM a = ButtplugM {runButtplugM :: ReaderT (Handle, TVar Int) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadThrow,
      MonadIO,
      MonadReader (Handle, TVar Int),
      MonadUnliftIO,
      MonadBase IO
    )

instance MonadBaseControl IO ButtplugM where
  type StM ButtplugM a = a
  liftBaseWith f = ButtplugM $ liftBaseWith $ \q -> f (q . runButtplugM)
  restoreM = ButtplugM . restoreM

runButtplug :: Handle -> ButtplugM a -> IO a
runButtplug handle bp = do
  msgIndexCounter <- newTVarIO 0
  flip runReaderT (handle, msgIndexCounter) . runButtplugM $ bp

sendMessages :: [Message] -> ButtplugM ()
sendMessages msgs = withHandle $ \h -> Handle.sendMessages h msgs

sendMessage :: Message -> ButtplugM ()
sendMessage msg = withHandle $ \h -> Handle.sendMessage h msg

receiveMessages :: ButtplugM [Message]
receiveMessages = withHandle Handle.receiveMessages

getHandle :: ButtplugM Handle
getHandle = fst <$> ask

withHandle :: (Handle -> IO a) -> ButtplugM a
withHandle k = getHandle >>= liftIO . k
