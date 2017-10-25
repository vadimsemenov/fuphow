module Days
       ( Day (..)
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       ) where

import           Data.List  (elemIndex)
import           Data.Maybe (fromJust)


data Day = MON | TUE | WED | THU | FRI | SAT | SUN
    deriving (Show, Eq, Enum)

days  :: [Day]
days  = [MON, TUE, WED, THU, FRI, SAT, SUN]

nextDay :: Day -> Day
nextDay today
    | today == SUN = MON
    | otherwise    = succ today

afterDays :: Day -> Int -> Day
afterDays currentDay n
    | n < 0     = error "Negative number of days"
    | otherwise = iterate nextDay currentDay !! (n `mod` 7)

isWeekend :: Day -> Bool
isWeekend SAT = True
isWeekend SUN = True
isWeekend _   = False

daysToParty :: Day -> Int
daysToParty today = fromJust $ elemIndex FRI $ iterate nextDay today
