module Types.Event where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Time (UTCTime)
import Deriving.Aeson
import Types.EventType
import Types.Payloads

data Event = Event
  { eventId :: Text
  , createdAt :: UTCTime
  , payload :: Payload
  , eventType :: EventType
  }
  deriving stock (Eq, Show, Read, Ord, Generic)

parsePayload :: EventType -> Value -> Parser Payload
parsePayload eventType value =
  withObject
    "payload"
    ( \p ->
        case eventType of
          IdCardRegisteredEvent -> IdRegistered <$> parseJSON (Object p)
          IdCardScannedAtEntranceGateEvent -> IdScannedAtEntrance <$> parseJSON (Object p)
          IdCardScannedAtExitGateEvent -> IdScannedAtExit <$> parseJSON (Object p)
          FractionWasDroppedEvent -> FractionWasDropped <$> parseJSON (Object p)
          PriceWasCalculatedEvent -> PriceCalculated <$> parseJSON (Object p)
          ExemptionWasGrantedEvent -> ExemptionWasGranted <$> parseJSON (Object p)
          DiscountWasBoughtEvent -> DiscountWasBought <$> parseJSON (Object p)
    )
    value

serialisePayload :: Payload -> Value
serialisePayload = \case
  IdRegistered p -> toJSON p
  IdScannedAtEntrance p -> toJSON p
  IdScannedAtExit p -> toJSON p
  FractionWasDropped p -> toJSON p
  PriceCalculated p -> toJSON p
  ExemptionWasGranted p -> toJSON p
  DiscountWasBought p -> toJSON p

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    eventId <- o .: "event_id"
    createdAt <- o .: "created_at"
    eventType <- o .: "type"
    payloadObject <- o .: "payload"
    payload <- parsePayload eventType payloadObject
    pure Event{..}

instance ToJSON Event where
  toJSON Event{..} =
    object
      [ "event_id" .= eventId
      , "created_at" .= createdAt
      , "payload" .= serialisePayload payload
      , "type" .= eventType
      ]
