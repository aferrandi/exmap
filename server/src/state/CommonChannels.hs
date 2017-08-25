module CommonChannels where

import EventMessages (EventChan)
import LoadMessages (LoadChan)
import StoreMessages (StoreChan)
import LogMessages (LogChan)

data CommonChans = CommonChans {
    eventChan :: EventChan,
    loadChan :: LoadChan,
    storeChan :: StoreChan,
    logChan :: LogChan
}