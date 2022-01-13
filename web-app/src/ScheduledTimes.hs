module ScheduledTimes
  ( ScheduledTimes(..)
  , none

  , ScheduleableTime(..)
  , fromList
  , toList
  )
where

import Data.Foldable (foldl')

data ScheduledTimes = ScheduledTimes
  { h0 :: Bool
  , h1 :: Bool
  , h2 :: Bool
  , h3 :: Bool
  , h4 :: Bool
  , h5 :: Bool
  , h6 :: Bool
  , h7 :: Bool
  , h8 :: Bool
  , h9 :: Bool
  , h10 :: Bool
  , h11 :: Bool
  , h12 :: Bool
  , h13 :: Bool
  , h14 :: Bool
  , h15 :: Bool
  , h16 :: Bool
  , h17 :: Bool
  , h18 :: Bool
  , h19 :: Bool
  , h20 :: Bool
  , h21 :: Bool
  , h22 :: Bool
  , h23 :: Bool
  }

none :: ScheduledTimes
none = ScheduledTimes
  False False False False False False False False False False False False
  False False False False False False False False False False False False

data ScheduleableTime
  = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 | H11
  | H12 | H13 | H14 | H15 | H16 | H17 | H18 | H19 | H20 | H21 | H22 | H23
  deriving (Show, Eq, Ord, Bounded, Enum)

fromList :: [ScheduleableTime] -> ScheduledTimes
fromList scheduled_times =
  foldl'
    (\times t ->
      case t of
        H0 -> times { h0 = True }
        H1 -> times { h1 = True }
        H2 -> times { h2 = True }
        H3 -> times { h3 = True }
        H4 -> times { h4 = True }
        H5 -> times { h5 = True }
        H6 -> times { h6 = True }
        H7 -> times { h7 = True }
        H8 -> times { h8 = True }
        H9 -> times { h9 = True }
        H10 -> times { h10 = True }
        H11 -> times { h11 = True }
        H12 -> times { h12 = True }
        H13 -> times { h13 = True }
        H14 -> times { h14 = True }
        H15 -> times { h15 = True }
        H16 -> times { h16 = True }
        H17 -> times { h17 = True }
        H18 -> times { h18 = True }
        H19 -> times { h19 = True }
        H20 -> times { h20 = True }
        H21 -> times { h21 = True }
        H22 -> times { h22 = True }
        H23 -> times { h23 = True })
    none
    scheduled_times

toList :: ScheduledTimes -> [ScheduleableTime]
toList (ScheduledTimes h00 h01 h02 h03 h04 h05 h06 h07 h08 h09 h10 h11
                       h12 h13 h14 h15 h16 h17 h18 h19 h20 h21 h22 h23) =
  filter
    (\x -> case x of
      H0 -> h00
      H1 -> h01
      H2 -> h02
      H3 -> h03
      H4 -> h04
      H5 -> h05
      H6 -> h06
      H7 -> h07
      H8 -> h08
      H9 -> h09
      H10 -> h10
      H11 -> h11
      H12 -> h12
      H13 -> h13
      H14 -> h14
      H15 -> h15
      H16 -> h16
      H17 -> h17
      H18 -> h18
      H19 -> h19
      H20 -> h20
      H21 -> h21
      H22 -> h22
      H23 -> h23)
    [H0 .. H23]