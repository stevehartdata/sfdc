module ScheduledDays
  ( ScheduledDays(..)
  , none

  , ScheduleableDay(..)
  , fromList
  , toList
  )

where

import Data.Foldable (foldl')

data ScheduledDays = ScheduledDays
  { monday :: Bool
  , tuesday :: Bool
  , wednesday :: Bool
  , thursday :: Bool
  , friday :: Bool
  , saturday :: Bool
  }

none :: ScheduledDays
none = ScheduledDays False False False False False False

data ScheduleableDay
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Bounded, Enum)

fromList :: [ScheduleableDay] -> ScheduledDays
fromList scheduled_days =
  foldl'
    (\days d ->
      case d of
        Monday -> days { monday = True}
        Tuesday -> days { tuesday = True }
        Wednesday -> days { wednesday = True }
        Thursday -> days { thursday = True }
        Friday -> days { friday = True }
        Saturday -> days { saturday = True })
    none
    scheduled_days

toList :: ScheduledDays -> [ScheduleableDay]
toList (ScheduledDays m t w r f s) =
  filter
    (\x -> case x of
      Monday -> m
      Tuesday -> t
      Wednesday -> w
      Thursday -> r
      Friday -> f
      Saturday -> s)
    [Monday .. Saturday]