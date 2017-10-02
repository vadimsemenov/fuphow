module Days
       ( Day (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)


data Day = MON | TUE | WED | THU | FRI | SAT | SUN
  deriving (Show, Eq)

days  = [MON, TUE, WED, THU, FRI, SAT, SUN]
days' = cycle days

nextDay :: Day -> Day
nextDay currentDay = fromJust $ lookup currentDay $ zip days' (tail days')

afterDays :: Day -> Int -> Day
afterDays currentDay n
  | n < 0 = error "Negative number of days"
  | otherwise = (dropWhile (/= currentDay) days') !! n

isWeekend :: Day -> Bool
isWeekend SAT = True
isWeekend SUN = True
isWeekend _   = False

daysToParty :: Day -> Int
daysToParty today = fromJust $ elemIndex FRI $ dropWhile (/= today) days'