module Validation where

import Data.List qualified as List

import Types

validate :: Body -> Maybe Body
validate b = do
  idCardWasRegistered b.history
  idCardWasScannedWhenEntering b.history
  idCardWasScannedWhenExiting b.history
  pure b

idCardWasRegistered :: [Event] -> Maybe Event
idCardWasRegistered history = List.find (\e -> e.eventType == IdCardRegistered) history

idCardWasScannedWhenEntering :: [Event] -> Maybe Event
idCardWasScannedWhenEntering history = List.find (\e -> e.eventType == IdCardScannedAtEntranceGate) history

idCardWasScannedWhenExiting :: [Event] -> Maybe Event
idCardWasScannedWhenExiting history = List.find (\e -> e.eventType == IdCardScannedAtEntranceGate) history
