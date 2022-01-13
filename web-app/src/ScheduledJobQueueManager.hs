{-# LANGUAGE NamedFieldPuns #-}

module ScheduledJobQueueManager
  ()
where

-- Alarm libraries
import Control.Concurrent.AlarmClock (AlarmClock, TimeScale, withAlarmClock,
  setAlarm)

-- Time-related libraries
import Data.Hourglass
import Data.Time (UTCTime)
import Time.Types (DateTime (DateTime))
import Time.System (dateCurrent)

-- Miscellaneous libraries
import Database.Persist.Sql (ConnectionPool)
import Control.Monad.IO.Class (liftIO)

import Queue.Types (BackupQueue)
import Worker (Worker, waitForTerminationAction)

hourFloor :: DateTime -> DateTime
hourFloor dt@(DateTime {dtTime}) =
  dt { dtTime = dtTime { todMin = 0, todSec = 0, todNSec = 0 } }

currentHour :: IO DateTime
currentHour = hourFloor <$> dateCurrent

nextHour :: IO DateTime
nextHour = do
  current_hour <- currentHour
  return $ current_hour `timeAdd` Hours 1

run :: ConnectionPool -> BackupQueue -> Worker ()
run db_conn_pool q = do
  waitForTermination <- waitForTerminationAction
  liftIO $ withAlarmClock
    (\alarm_clock _alarm_scheduled_time -> do
      setAlarmForNextHour alarm_clock
      current_slot <- getCurrentTimeSlot
      enqueueJobs current_slot)
    (\alarm_clock -> do
      setAlarmForNextHour alarm_clock
      waitForTermination)

setAlarmForNextHour :: AlarmClock UTCTime -> IO ()
setAlarmForNextHour alarm_clock = do
  next_hour <- nextHour
  setAlarm alarm_clock (toUTCTime next_hour)

toUTCTime :: DateTime -> UTCTime
toUTCTime = undefined

getCurrentTimeSlot :: IO ()
getCurrentTimeSlot = undefined

enqueueJobs :: () -> IO ()
enqueueJobs = undefined