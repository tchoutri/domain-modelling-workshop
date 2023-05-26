module Validation where

import Control.Monad
import Data.List qualified as List
import Data.Maybe

import Types

validate :: Body -> Maybe Body
validate b = do
  guard (idCardWasRegistered b.history)
  guard (idCardWasScannedWhenEntering b.history)
  guard (idCardWasScannedWhenExiting b.history)
  pure b

idCardWasRegistered :: [Event] -> Bool
idCardWasRegistered history = isJust $ List.find (\e -> e.eventType == IdCardRegistered) history

idCardWasScannedWhenEntering :: [Event] -> Bool
idCardWasScannedWhenEntering history = isJust $ List.find (\e -> e.eventType == IdCardScannedAtEntranceGate) history

idCardWasScannedWhenExiting :: [Event] -> Bool
idCardWasScannedWhenExiting history = isJust $ List.find (\e -> e.eventType == IdCardScannedAtEntranceGate) history
