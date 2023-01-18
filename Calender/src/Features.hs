module Features where

import DateTime
import Calendar
import Text.PrettyPrint.Boxes


-- Exercise 9
-- number of events of a calender.
countEvents :: Calendar -> Int
countEvents c = length (events c)

-- for all the events we check whether the given time is in between or not.
findEvents :: DateTime -> Calendar -> [Event]
findEvents dt c = filter (\e -> (dtstart e <= dt) && (dt < dtend e)) (events c)

-- an event is overlapping with another even if the start time of the event is in between
-- the start time / end time of another event. (we can use the findEvents to check this out).
-- moreover each event is overlapping with itself obviously. So if the length of the resulting list
-- with the findEvents function is higher than 1 then we have overlapping events.
checkOverlapping :: Calendar -> Bool
checkOverlapping c = any (\e -> length (findEvents (dtstart e) c) > 1) (events c)

timeSpent :: String -> Calendar -> Int
timeSpent s c = sum (map (\e -> difference (dtstart e) (dtend e)) xs)
    where xs = filter (\e -> equalSummary (summary e) s) (events c) -- xs is the list of all events with the given summary.
          equalSummary Nothing _  = False -- checks whether a summary of an event is equal to a string.
          equalSummary (Just x) y = x == y

-- below gives the difference of the start time and end time of an event.
-- this is an approximate because a year can have more minutes depending on whether
-- it is a leap year or not. Same goes for month. (Even days differ in the number of minutes)
-- anyways taking into account the whole thing would have taken a long time.
-- Morover, most events occur over a span of days so this would suffice (I think at least).
-- Lastly, the number of seconds has been neglected but it could have been written as:
-- runSecond (second (time x)) - runSecond(second (time y)) * (1.0/60) ; because each second is 1.0/60 minute
-- in that case we had to convert each of the integers to a float.

difference :: DateTime -> DateTime -> Int
difference x y = runYear   (year d)   - runYear  (year d)   * 525600 + -- there are 525600 minutes per year.
                 runMonth  (month d)  - runMonth (month d)  * 43800  + -- there are 43800 minutes per year.
                 runDay    (day d)    - runDay   (day d)    * 1440   + -- there are 1440 minutes per year.
                 runHour   (hour t)   - runHour  (hour t)   * 60     +
                 runMinute (minute t) - runMinute(minute t)
                  where t = time x
                        d = date x  
-- Exercise 10
ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = show (emptyBox ((daysInMonth y m) `div` 4) 7)

-- we get the number of days in a month.
daysInMonth :: Year -> Month -> Int
daysInMonth year month 
  | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | m == 2 && leapYear y = 29
  | otherwise = 28 -- it is not a leap year
    where leapYear y = (mod y 400 == 0) || (mod y 4 == 0 && not (mod y 100 == 0))
          m = runMonth month
          y = runYear year
