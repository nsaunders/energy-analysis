module EnergyUsage.Time (addHours) where

import Prelude
import Data.Int (toNumber)
import Data.Time (Time, adjust)
import Data.Time.Duration (Hours(..))
import Data.Tuple (snd)

addHours :: Int -> Time -> Time
addHours n = snd <<< adjust (Hours $ toNumber n)
