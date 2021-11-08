module EnergyUsage.Parser (parseUsage) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Date (Day, Month(..), Year, exactDate)
import Data.Either (Either)
import Data.Enum (toEnum)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.String.CodeUnits (fromCharArray)
import Data.Time (Hour)
import Data.Unfoldable (replicateA)
import EnergyUsage.Data (Entry)
import Text.Parsing.StringParser (ParseError, Parser, fail, runParser)
import Text.Parsing.StringParser.CodeUnits (anyChar, anyDigit, char, noneOf, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (between, many, many1)

parseUsage :: String -> Either ParseError (Array Entry)
parseUsage = runParser usage

usage :: Parser (Array Entry)
usage = do
  skipHeader
  entries <- many1 entry
  pure $ fromFoldable entries

entry :: Parser Entry
entry = do
  m <- month # between (char '"') (char '/')
  d <- day
  _ <- char '/'
  y <- year
  _ <- string "\","
  skipSpaces
  h <- hour # between (char '"') (char '"')
  _ <- char ','
  u <- number # between (char '"') (char '"')
  _ <- char ','
  d' <- number # between (char '"') (char '"')
  case exactDate y m d of
    Nothing ->
      fail "Invalid date"
    Just date -> do
      skipLine
      pure { date, hour: h, usage: u, demand: d' }

month :: Parser Month
month =
  (string "01" *> pure January) <|>
  (string "02" *> pure February) <|>
  (string "03" *> pure March) <|>
  (string "04" *> pure April) <|>
  (string "05" *> pure May) <|>
  (string "06" *> pure June) <|>
  (string "07" *> pure July) <|>
  (string "08" *> pure August) <|>
  (string "09" *> pure September) <|>
  (string "10" *> pure October) <|>
  (string "11" *> pure November) <|>
  (string "12" *> pure December)

int :: Int -> Parser Int
int n = do
  digits :: Array Char <- replicateA n anyDigit
  case Int.fromString $ fromCharArray $ fromFoldable digits of
    Nothing -> fail "Invalid number"
    Just x -> pure x

day :: Parser Day
day = do
  maybeDay <- toEnum <$> int 2
  case maybeDay of
    Nothing -> fail "Invalid day"
    Just d -> pure d

year :: Parser Year
year = do
  maybeYear <- toEnum <$> int 4
  case maybeYear of
    Nothing -> fail "Invalid year"
    Just y -> pure y

hour :: Parser Hour
hour = do
  maybeHour <- Int.fromString <<< fromCharArray <<< fromFoldable <$> many1 anyDigit
  case maybeHour of
    Nothing -> fail "Invalid hour"
    Just h -> do
      _ :: Array _ <- replicateA 4 anyChar
      ampm <- string "AM" <|> string "PM"
      skipSpaces
      let h' = if ampm == "AM" then (if h == 12 then 0 else h) else (if h == 12 then 12 else h + 12)
      case toEnum h' of
        Nothing -> fail "Invalid hour"
        Just h'' -> pure h''

number :: Parser Number
number = do
  s <- many1 $ anyDigit <|> char '.'
  case Number.fromString $ fromCharArray $ fromFoldable s of
    Nothing -> fail "Invalid number"
    Just n -> pure n

skipHeader :: Parser Unit
skipHeader = do
  _ :: Array _ <- replicateA 3 skipLine
  pure unit

skipLine :: Parser Unit
skipLine = do
  _ <- many $ noneOf ['\r', '\n']
  skipSpaces
