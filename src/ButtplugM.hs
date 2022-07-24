{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ButtplugM where

-- start with a concrete monad, we'll figure out how to do a transformer later

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Buttplug.Core.Handle (Handle)
import Buttplug.Core.Handle qualified as Handle
import Data.IORef (IORef)
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Buttplug.Core (Message)

-- Eventually we'll automatically handle giving messages correct message ids.
-- We'll present users with an api where they don't need to worry about id
newtype ButtplugM a = ButtplugM { runButtplugM :: ReaderT (Handle, TVar Int) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Handle, TVar Int))

runButtplug :: Handle -> ButtplugM a -> IO a
runButtplug handle bp = do
  msgIndexCounter <- newTVarIO 0
  flip runReaderT (handle, msgIndexCounter) . runButtplugM $ bp

sendMessages :: [Message] -> ButtplugM ()
sendMessages msgs = withHandle $ \h -> Handle.sendMessages h msgs

sendMessage :: Message -> ButtplugM ()
sendMessage msg = withHandle $ \h -> Handle.sendMessage h msg

receiveMessages :: ButtplugM [Message]
receiveMessages = withHandle $ Handle.receiveMessages

handle :: ButtplugM Handle
handle = fst <$> ask

withHandle :: (Handle -> IO a) -> ButtplugM a
withHandle k = handle >>= liftIO . k
