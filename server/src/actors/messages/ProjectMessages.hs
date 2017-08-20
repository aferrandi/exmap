module ProjectMessages where

import Control.Concurrent.STM.TChan (TChan)

import XMapTypes
import LogTypes
import Project

data ProjectMessage = PMMap XNamedMap
                      | PNUpdateProject Project
                      | PMLog Log
                      | PMStop

type ProjectChan = TChan ProjectMessage