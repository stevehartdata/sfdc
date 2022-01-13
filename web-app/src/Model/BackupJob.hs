module Model.BackupJob
  ( scheduledDays
  , scheduledTimes
  )
where

import Model
import ScheduledDays
import ScheduledTimes

scheduledDays bj =
  ScheduledDays
    (backupJobScheduledMonday bj)
    (backupJobScheduledTuesday bj)
    (backupJobScheduledWednesday bj)
    (backupJobScheduledThursday bj)
    (backupJobScheduledFriday bj)
    (backupJobScheduledSaturday bj)

scheduledTimes bj =
  ScheduledTimes
    (backupJobScheduled0 bj)
    (backupJobScheduled1 bj)
    (backupJobScheduled2 bj)
    (backupJobScheduled3 bj)
    (backupJobScheduled4 bj)
    (backupJobScheduled5 bj)
    (backupJobScheduled6 bj)
    (backupJobScheduled7 bj)
    (backupJobScheduled8 bj)
    (backupJobScheduled9 bj)
    (backupJobScheduled10 bj)
    (backupJobScheduled11 bj)
    (backupJobScheduled12 bj)
    (backupJobScheduled13 bj)
    (backupJobScheduled14 bj)
    (backupJobScheduled15 bj)
    (backupJobScheduled16 bj)
    (backupJobScheduled17 bj)
    (backupJobScheduled18 bj)
    (backupJobScheduled19 bj)
    (backupJobScheduled20 bj)
    (backupJobScheduled21 bj)
    (backupJobScheduled22 bj)
    (backupJobScheduled23 bj)
