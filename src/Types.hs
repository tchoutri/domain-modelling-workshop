{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Deriving.Aeson
import Web.Twain.Types

data CommandType
  = CalculatePrice
  | LOL
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[SumUntaggedValue] CommandType

instance ParsableParam CommandType where
  parseParam = readEither

data Body = Body
  { command :: Command
  , history :: [Event]
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass
    (ToJSON, FromJSON)

instance ParsableParam Body where
  parseParam = readEither

data Command = Command
  { commandId :: Text
  , createdAt :: UTCTime
  , payload :: Payload
  , commandType :: CommandType
  }
  deriving stock (Eq, Show, Read, Ord, Generic)

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    commandId <- o .: "command_id"
    createdAt <- o .: "created_at"
    payload <- o .: "payload"
    commandType <- o .: "type"
    pure Command{..}

instance ToJSON Command where
  toJSON Command{..} =
    object
      [ "command_id" .= commandId
      , "createdAt" .= createdAt
      , "payload" .= payload
      , "type" .= commandType
      ]

instance ParsableParam Command where
  parseParam = readEither

data Payload = Payload
  { cardId :: Text
  , address :: Maybe Text
  , city :: Maybe Text
  , personId :: Maybe Text
  , date :: Maybe Day
  , fractionType :: Maybe Text
  , weight :: Maybe Word
  , priceAmount :: Maybe Float
  , priceCurrency :: Maybe Currency
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] Payload

defaultPayload :: Payload
defaultPayload =
  Payload
    { cardId = ""
    , address = Nothing
    , city = Nothing
    , personId = Nothing
    , date = Nothing
    , fractionType = Nothing
    , weight = Nothing
    , priceAmount = Nothing
    , priceCurrency = Nothing
    }

instance ParsableParam Payload where
  parseParam = readEither

data Currency
  = EUR
  | LOL2
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[SumUntaggedValue] Currency

data Event = Event
  { eventId :: Text
  , createdAt :: UTCTime
  , payload :: Payload
  , eventType :: EventType
  }
  deriving stock (Eq, Show, Read, Ord, Generic)

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    eventId <- o .: "event_id"
    createdAt <- o .: "created_at"
    payload <- o .: "payload"
    eventType <- o .: "type"
    pure Event{..}

instance ToJSON Event where
  toJSON Event{..} =
    object
      [ "event_id" .= eventId
      , "created_at" .= createdAt
      , "payload" .= payload
      , "type" .= eventType
      ]

data EventType
  = IdCardRegistered
  | IdCardScannedAtEntranceGate
  | IdCardScannedAtExitGate
  | FractionWasDropped
  | PriceWasCalculated
  | ExemptionWasGranted
  | DiscountWasBought
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass
    (ToJSON, FromJSON)
