module ButtPipe where

import Control.Monad (unless, forever)
import Pipes
import Buttplug.Core


buttplugMessage :: Connector c => Connection c -> Producer Message IO ()
buttplugMessage con = forever $ do
  msgs <- lift $ receiveMsgs con
  mapM_ yield msgs


