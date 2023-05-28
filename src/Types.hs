{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Aeson
import Deriving.Aeson
import Types.Command
import Types.Event
import Web.Twain.Types

data Body = Body
  { command :: Command
  , history :: [Event]
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass
    (ToJSON, FromJSON)

instance ParsableParam Body where
  parseParam = readEither
