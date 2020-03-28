module DateValue where

import Data.Time.Calendar
import Data.Time
import Data.Fixed

mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

mkUTCDate :: (Integer, Int, Int)
          -> UTCTime
mkUTCDate (year, mon, day) =
  UTCTime (fromGregorian year mon day)
           (secondsToDiffTime 0)
