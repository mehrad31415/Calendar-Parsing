module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)

-- based on notes: 
-- data Parser symbol result = [symbol] -> [(result,[symbol])]

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUTC

-- checks whether there is a Z; if yes returns True; else False. 
-- Given the data typde of DateTime the last field (utc) is bool.
parseUTC :: Parser Char Bool
parseUTC =  succeed True <* symbol 'Z'
        <|> succeed False

-- the parse Hour, Minute, Second are used for parseTime.
parseHour :: Parser Char Hour
parseHour = (\a b -> Hour $ read [a, b]) <$> digit <*> digit

parseMinute :: Parser Char Minute
parseMinute = (\a b -> Minute $ read [a, b]) <$> digit <*> digit

parseSecond :: Parser Char Second
parseSecond = (\a b -> Second $ read [a, b]) <$> digit <*> digit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

-- the parse day, month, year are used for parseDate.
-- a day contains two digits, so does a month, however, a year contains 4 digits. 
parseDay :: Parser Char Day
parseDay = (\a b -> Day $ read [a, b]) <$> digit <*> digit

parseMonth :: Parser Char Month
parseMonth = (\a b -> Month $ read [a, b]) <$> digit <*> digit

parseYear :: Parser Char Year
parseYear = (\a b c d -> Year $ read [a, b, c, d]) <$> 
             digit <*> digit <*> digit <*> digit

-- there is a separator between date and time: "T" where we just check its presence
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay <* symbol 'T'

-- Exercise 2
-- parse :: Parser s a -> [s] -> [(a, [s])]

run :: Parser a b -> [a] -> Maybe b
run p xs = checkComplete (parse p xs)
   where checkComplete [] = Nothing
         checkComplete ((r,xs):ys)
           | null xs = Just r -- xs is the remaining string of each parsing result.
           | otherwise = checkComplete ys

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime x = show (runYear   (year (date x))) ++
                  show (runMonth  (month (date x)))++
                  show (runDay    (day (date x)))  ++
                  "T" ++
                  show (runHour   (hour   (time x)))   ++
                  show (runMinute (minute (time x)))   ++
                  show (runSecond (second (time x)))   ++
                  (if utc x then "Z" else "")
                
-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime x = runMonth (month d)   <= 12 && runMonth  (month d)  >= 1 &&
                  runHour  (hour t)    <= 23 && runHour   (hour t)   >= 0 &&
                  runMinute (minute t) <= 59 && runMinute (minute t) >= 0 &&
                  runSecond (second t) <= 59 && runSecond (second t) >= 0 &&
                  checkingDaysOfMonth x      && runYear   (year d)   >= 0 
                    where d = date x
                          t = time x

-- checks the number of days in a month whether it is valid according to the
-- month or not. It also takes into account leap years. 
-- one of the below situations has to be true thats why we have put an "or" between each condition.
checkingDaysOfMonth :: DateTime -> Bool
checkingDaysOfMonth x = 
  (m == 1  && d <=31 && d >=0) ||
  (m == 2  && d <=28 && d >=0) ||
  (m == 3  && d <=31 && d >=0) ||
  (m == 4  && d <=30 && d >=0) ||
  (m == 5  && d <=31 && d >=0) ||
  (m == 6  && d <=30 && d >=0) ||
  (m == 7  && d <=31 && d >=0) ||
  (m == 8  && d <=31 && d >=0) ||
  (m == 9  && d <=30 && d >=0) ||
  (m == 10 && d <=31 && d >=0) ||
  (m == 11 && d <=30 && d >=0) ||
  (m == 12 && d <=31 && d >=0) ||
  (m == 2  && d == 29 && leapYear y) -- considers leap years
     where m = runMonth (month (date x))
           d = runDay   (day   (date x))
           y = runYear  (year  (date x))
           leapYear y = (mod y 400 == 0) || (mod y 4 == 0 && not (mod y 100 == 0))
