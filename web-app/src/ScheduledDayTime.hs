module ScheduledDayTime
  ()
where

import ScheduledDay (ScheduledDay)
import ScheduledTime (ScheduledTime)

data ScheduledDayTime = ScheduledDayTime
  { scheduledDay :: ScheduledDay
  , scheduledTime :: ScheduledTime
  }