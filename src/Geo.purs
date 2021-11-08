module EnergyUsage.Geo (Coordinates, Latitude, Longitude, mkCoordinates, mkLatitude, mkLongitude, latitude, longitude) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype Latitude = Latitude Number

newtype Longitude = Longitude Number

newtype Coordinates = Coordinates (Tuple Latitude Longitude)

mkLatitude :: Number -> Maybe Latitude
mkLatitude n
  | n >= -90.0 && n <= 90.0 = Just $ Latitude n
  | otherwise = Nothing

mkLongitude :: Number -> Maybe Longitude
mkLongitude n
  | n >= -180.0 && n <= 180.0 = Just $ Longitude n
  | otherwise = Nothing

mkCoordinates :: Latitude -> Longitude -> Coordinates
mkCoordinates lat lon = Coordinates $ Tuple lat lon

latitude :: Coordinates -> Latitude
latitude (Coordinates (Tuple lat _)) = lat

longitude :: Coordinates -> Longitude
longitude (Coordinates (Tuple _ lon)) = lon
