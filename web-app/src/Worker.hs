{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Worker
  ( Worker
  , waitForTerminationAction
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask)

newtype Worker a = Worker (ReaderT (IO ()) IO a)
  deriving (Applicative, Functor, Monad, MonadIO)

waitForTerminationAction :: Worker (IO ())
waitForTerminationAction = Worker $ ask