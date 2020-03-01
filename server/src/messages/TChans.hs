module TChans where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

sendToAll :: [TChan a] -> a -> STM ()
sendToAll chs xm = mapM_ sendToOne chs
    where sendToOne ch = writeTChan ch xm
