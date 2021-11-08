module EnergyUsage.Data where

import Prelude

import Data.Array (foldl)
import Data.Date (Date, Day, Month(..), Weekday(..), day, month, weekday, year)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (fromJust)
import Data.Time (Hour, hour, minute)
import EnergyUsage.Geo (mkCoordinates, mkLatitude, mkLongitude)
import EnergyUsage.Sun (sunrise, sunset)
import EnergyUsage.Time (addHours)
import Partial.Unsafe (unsafePartial)

type Entry =
  { date :: Date
  , hour :: Hour
  , usage :: Number
  , demand :: Number
  }

orderEntriesByDemand :: Entry -> Entry -> Ordering
orderEntriesByDemand a b
  | a.demand < b.demand = LT
  | a.demand > b.demand = GT
  | otherwise = EQ

orderEntriesByMonth :: Entry -> Entry -> Ordering
orderEntriesByMonth a b
  | year a.date < year b.date = LT
  | year a.date > year b.date = GT
  | month a.date < month b.date = LT
  | month a.date > month b.date = GT
  | otherwise = EQ

hours :: { h15 :: Hour, h19 :: Hour }
hours =
  let
    mk h = unsafePartial $ fromJust $ toEnum h
  in
    { h15: mk 15, h19: mk 19 }

days
  :: { d1 :: Day
     , d2 :: Day
     , d3 :: Day
     , d4 :: Day
     , d5 :: Day
     , d7 :: Day
     , d10 :: Day
     , d11 :: Day
     , d12 :: Day
     , d15 :: Day
     , d21 :: Day
     , d22 :: Day
     , d24 :: Day
     , d25 :: Day
     , d26 :: Day
     , d28 :: Day
     , d30 :: Day
     , d31 :: Day
     }
days =
  let
    mk d = unsafePartial $ fromJust $ toEnum d
  in
    { d1: mk 1
    , d2: mk 2
    , d3: mk 3
    , d4: mk 4
    , d5: mk 5
    , d7: mk 7
    , d10: mk 10
    , d11: mk 11
    , d12: mk 12
    , d15: mk 15
    , d21: mk 21
    , d22: mk 22
    , d24: mk 24
    , d25: mk 25
    , d26: mk 26
    , d28: mk 28
    , d30: mk 30
    , d31: mk 31
    }

isWeekend :: Date -> Boolean
isWeekend d = weekday d == Saturday || weekday d == Sunday

isNewYearsDay :: Date -> Boolean
isNewYearsDay d
  | isWeekend d = false
  | otherwise =
      case month d of
        December ->
          day d == days.d31 && weekday d == Friday
        January ->
          day d == days.d1 || (day d == days.d2 && weekday d == Monday)
        _ ->
          false

isThirdMonday :: Date -> Boolean
isThirdMonday d
  | weekday d /= Monday = false
  | otherwise = day d >= days.d15 && day d <= days.d21

isMLKDay :: Date -> Boolean
isMLKDay d = isThirdMonday d && month d == January

isPresidentsDay :: Date -> Boolean
isPresidentsDay d = isThirdMonday d && month d == February

isCesarChavezDay :: Date -> Boolean
isCesarChavezDay d
  | isWeekend d = false
  | otherwise =
      case month d of
        March ->
          (day d == days.d30 && weekday d == Friday) || day d == days.d31
        April ->
          day d == days.d1 && weekday d == Monday
        _ ->
          false

isMemorialDay :: Date -> Boolean
isMemorialDay d = month d == May && weekday d == Monday && day d >= days.d25

isIndependenceDay :: Date -> Boolean
isIndependenceDay d
  | isWeekend d || month d /= July = false
  | otherwise = (day d == days.d3 && weekday d == Friday) || day d == days.d4 || (day d == days.d5 && weekday d == Monday)

isLaborDay :: Date -> Boolean
isLaborDay d = month d == September && weekday d == Monday && day d <= days.d7

isVeteransDay :: Date -> Boolean
isVeteransDay d
  | month d /= November || isWeekend d = false
  | otherwise = (day d == days.d10 && weekday d == Friday) || day d == days.d11 || (day d == days.d12 && weekday d == Monday)

isThanksgiving :: Date -> Boolean
isThanksgiving d = month d == November && weekday d == Thursday && day d >= days.d22 && day d <= days.d28

isChristmas :: Date -> Boolean
isChristmas d
  | isWeekend d = false
  | month d /= December = false
  | otherwise = (day d == days.d24 && weekday d == Friday) || day d == days.d25 || (day d == days.d26 && weekday d == Monday)

isPeakDate :: Date -> Boolean
isPeakDate = not <<< foldl (||) false <<< flap ps
  where
    ps =
      [ isWeekend
      , isNewYearsDay
      , isMLKDay
      , isPresidentsDay
      , isCesarChavezDay
      , isMemorialDay
      , isIndependenceDay
      , isLaborDay
      , isVeteransDay
      , isThanksgiving
      , isChristmas
      ]

isPeak :: Entry -> Boolean
isPeak { date, hour }
  | hour < hours.h15 || hour > hours.h19 = false
  | otherwise = isPeakDate date

afterSunrise :: Entry -> Boolean
afterSunrise e =
  let
    coordinates =
      unsafePartial
        $ fromJust
        $ mkCoordinates <$> mkLatitude 33.7 <*> mkLongitude (-112.15)
    sunrise' = addHours (-7) $ sunrise coordinates e.date
    cutoff = addHours (if fromEnum (minute sunrise') <= 15 then 1 else 2) $ sunrise'
  in
    e.hour >= hour cutoff

beforeSunset :: Entry -> Boolean
beforeSunset e =
  let
    coordinates =
      unsafePartial
        $ fromJust
        $ mkCoordinates <$> mkLatitude 33.7 <*> mkLongitude (-112.15)
    sunset' = addHours (-7) $ sunset coordinates e.date
    cutoff = addHours (if fromEnum (minute sunset') >= 45 then (-1) else (-2)) $ sunset'
  in
    e.hour <= hour cutoff

usageCharge :: Entry -> Number
usageCharge e
  | isPeak e = 0.08683 * e.usage
  | otherwise = 0.0523 * e.usage

offsetUsageCharge :: Number -> Entry -> Number
offsetUsageCharge offset e
  | afterSunrise e && beforeSunset e = max 0.0 $ usageCharge $ e { usage = e.usage - offset }
  | otherwise = usageCharge e

-- https://www.aps.com/en/Residential/Service-Plans/Compare-Service-Plans/Saver-Choice-Max
demandChargeRate :: Month -> Number
demandChargeRate m =
  if m <= April || m >= November
    then 12.239
    else 17.438

demandCharge :: Entry -> Number
demandCharge { date, demand } = demandChargeRate (month date) * demand

offsetDemandCharge :: Number -> Entry -> Number
offsetDemandCharge offset e
  | beforeSunset e = max 0.0 $ demandCharge $ e { demand = e.demand - offset }
  | otherwise = demandCharge e
