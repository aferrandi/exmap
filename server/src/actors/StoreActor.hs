module StoreActor where

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text.IO as TIO
import XMapTypes

import StoreMessages
import ProjectMessages
import SystemMessages
import Project
import View
import WebClients
import Store
import Errors

actorStore :: FilePath -> StoreChan -> IO ()
actorStore root chan = loop
    where loop = do
            msg <- atomically $ readTChan chan
            case msg of
                StMStoreProject source c p -> do
                    storeProjectInActor root source c p
                    loop
                StMStop -> return ()

storeProjectInActor :: FilePath -> SystemChan -> WAClient -> Project -> IO ()
storeProjectInActor root source c p = do
       mp <- storeProject root p
       case mp of
           Nothing -> atomically $ writeTChan source (SMEvent $ SEProjectStored c p)
           Just err -> atomically $ writeTChan source (SMEvent $ SEProjectStoreError c p err)