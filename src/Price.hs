module Price where

import Data.List qualified as List

import Types

computePrice :: [Event] -> Float
computePrice history =
  case List.find (\e -> e.eventType == FractionWasDropped) history of
    Nothing -> 0.0
    Just _ -> undefined
