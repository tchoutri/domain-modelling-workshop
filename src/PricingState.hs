module PricingState where

import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Debug.Trace
import Effectful
import Effectful.State.Static.Local

import Types.Event
import Types.EventType

data PricingState = PricingState
  { constructionWastePrice :: Float
  , greenWastePrice :: Float
  , constructionWasteExemption :: Int
  , greenWasteExemption :: Int
  , cardId :: Text
  }
  deriving stock (Eq, Ord, Show)

defaultPricingState :: PricingState
defaultPricingState =
  PricingState
    { constructionWastePrice = 0.15
    , greenWastePrice = 0.09
    , constructionWasteExemption = 0
    , greenWasteExemption = 0
    , cardId = "0"
    }

setDefaultParameters :: Eff '[State PricingState] ()
setDefaultParameters =
  modify
    ( \s ->
        s
          { constructionWastePrice = 0.15
          , greenWastePrice = 0.09
          , constructionWasteExemption = 0
          , greenWasteExemption = 0
          }
    )

setSouthParkParameters :: Eff '[State PricingState] ()
setSouthParkParameters =
  modify
    ( \s ->
        s
          { constructionWastePrice = 0.18
          , greenWastePrice = 0.12
          , constructionWasteExemption = 100
          , greenWasteExemption = 50
          }
    )

setParameters :: [Event] -> Eff '[State PricingState] ()
setParameters events = do
  traceShowM events
  case List.find (\e -> e.eventType == IdCardRegisteredEvent) events of
    Nothing -> error "uh-oh"
    Just e -> do
      modify (\s -> s{cardId = e.payload.cardId})
      case fromJust e.payload.city of
        "South Park" -> setSouthParkParameters
        _ -> setDefaultParameters
