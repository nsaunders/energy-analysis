module Main where

import Prelude

import Data.Array (filter, groupAllBy, length, zip)
import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Date (Date, Month, Year, day, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldMap, foldl, sum)
import Data.Int (toNumber)
import Data.Semigroup.Foldable (maximumBy)
import Data.Time (Hour)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import EnergyUsage.Data (Entry, demandCharge, isPeak, offsetDemandCharge, offsetUsageCharge, orderEntriesByDemand, orderEntriesByMonth, usageCharge)
import EnergyUsage.Parser (parseUsage)
import Math (round)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)

solarOutput :: Number
solarOutput = 7.1

main :: Effect Unit
main = launchAff_ do
  fileData <- readTextFile UTF8 "./usage.csv"
  liftEffect $ case parseUsage fileData of
    Left e ->
      liftEffect $ logShow e
    Right usage -> launchAff_ do
      writeTextFile UTF8 "./output.html" $
        "<link rel='stylesheet' href='tacit.css'>"
        <> usageChargeReport
        <> demandChargeReport
      liftEffect $ log "Report written to output.html"
         
      where
        usageChargeReport :: String
        usageChargeReport =
          table
          $ mempty
            <> caption (h2 "Usage Charge Savings")
            <> (thead
                $ tr
                  $ mempty
                    <> th "When"
                    <> th "Before"
                    <> th "After"
                    <> th "Savings"
               )
            <> tbody ((foldMap printRow $ zip entriesBefore entriesAfter) <> summary)

          where
            entriesBefore =
              usage' usageCharge <$> groupAllBy orderEntriesByMonth usage

            entriesAfter =
              (usage' $ offsetUsageCharge solarOutput) <$> groupAllBy orderEntriesByMonth usage

            usage' :: (Entry -> Number) -> NonEmptyArray Entry -> Tuple (Tuple Year Month) Number
            usage' calc entries =
              let
                e = head entries
                y = year e.date
                m = month e.date
              in
                Tuple (Tuple y m) $ foldl (+) 0.0 (calc <$> entries)

            printRow (Tuple (Tuple (Tuple y m) a) (Tuple _ b)) =
              tr
              $ mempty
                <> td (show m <> " " <> (show $ fromEnum y))
                <> td (printMoney a)
                <> td (printMoney b)
                <> td (printMoney $ a - b)

            summary :: String
            summary =
              let
                count = toNumber $ length entriesBefore
                before = (sum $ snd <$> entriesBefore) / count
                after = (sum $ snd <$> entriesAfter) / count
                average =
                  tr
                  $ mempty
                    <> td (strong "Average")
                    <> td (strong $ printMoney before)
                    <> td (strong $ printMoney after)
                    <> td (strong $ printMoney $ before - after)
                annualized =
                  tr
                  $ mempty
                    <> td (strong "Annualized")
                    <> td (strong $ printMoney $ before * 12.0)
                    <> td (strong $ printMoney $ after * 12.0)
                    <> td (strong $ printMoney $ (before - after) * 12.0)
              in
                average <> annualized

        demandChargeReport :: String
        demandChargeReport =
          table
          $ mempty
            <> caption (h2 "Demand Charge Savings")
            <> (thead
                $ tr
                  $ mempty
                    <> th "When"
                    <> th "Before"
                    <> th "After"
                    <> th "Savings"
               )
            <> tbody (foldMap printRow entries <> summary)

          where
            entries =
              maximumBy orderEntriesByDemand <$> (groupAllBy orderEntriesByMonth $ filter isPeak usage)

            printRow :: Entry -> String
            printRow entry =
              tr
              $ mempty
                <> td (printWhen entry.date entry.hour)
                <> td (printMoney $ demandCharge entry)
                <> td (printMoney $ offsetDemandCharge solarOutput entry)
                <> td (printMoney $ demandCharge entry - offsetDemandCharge solarOutput entry)

            summary :: String
            summary =
              let
                count = toNumber $ length entries
                before = (sum $ demandCharge <$> entries) / count
                after = (sum $ offsetDemandCharge solarOutput <$> entries) / count
                average =
                  tr
                  $ mempty
                    <> td (strong "Average")
                    <> td (strong $ printMoney before)
                    <> td (strong $ printMoney after)
                    <> td (strong $ printMoney $ before - after)
                annualized =
                  tr
                  $ mempty
                    <> td (strong "Annualized")
                    <> td (strong $ printMoney $ before * 12.0)
                    <> td (strong $ printMoney $ after * 12.0)
                    <> td (strong $ printMoney $ (before - after) * 12.0)
              in
                average <> annualized

        printWhen :: Date -> Hour -> String
        printWhen d h = show (month d) <> " " <> show (fromEnum $ day d) <> ", " <> show (fromEnum $ year d) <> " @ " <> show (fromEnum h) <> ":00"

        printMoney :: Number -> String
        printMoney m =
          if m == 0.0
            then "$0"
            else ("$" <> (show $ round (m * 100.0) / 100.0))

        wrapHTML :: String -> String -> String
        wrapHTML t x = "<" <> t <> ">" <> x <> "</" <> t <> ">"

        table :: String -> String
        table = wrapHTML "table"

        caption :: String -> String
        caption = wrapHTML "caption"

        thead :: String -> String
        thead = wrapHTML "thead"

        th :: String -> String
        th = wrapHTML "th"

        tbody :: String -> String
        tbody = wrapHTML "tbody"

        td :: String -> String
        td = wrapHTML "td"

        tr :: String -> String
        tr = wrapHTML "tr"

        h2 :: String -> String
        h2 = wrapHTML "h2"

        strong :: String -> String
        strong = wrapHTML "strong"
