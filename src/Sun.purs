module EnergyUsage.Sun where

import Prelude
import Data.DateTime (DateTime(..), Date, Time(..), time)
import Data.Enum (toEnum)
import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import EnergyUsage.Geo (Coordinates, Latitude, Longitude, latitude, longitude)

data Times

foreign import _getTimes :: JSDate -> Latitude -> Longitude -> Times

foreign import _sunset :: Times -> JSDate

foreign import _sunrise :: Times -> JSDate

midnight :: Time
midnight =
  unsafePartial
    $ fromJust
    $ Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

getTimes :: Coordinates -> Date -> Times
getTimes coords date =
  _getTimes
    (fromDateTime $ DateTime date midnight)
    (latitude coords)
    (longitude coords)

sunrise :: Coordinates -> Date -> Time
sunrise coords date =
  time
    $ unsafePartial
    $ fromJust
    $ toDateTime
    $ _sunrise
    $ getTimes coords date

sunset :: Coordinates -> Date -> Time
sunset coords date =
  time
    $ unsafePartial
    $ fromJust
    $ toDateTime
    $ _sunset
    $ getTimes coords date
