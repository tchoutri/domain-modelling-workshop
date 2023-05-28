module Types.EventType where

import Data.Aeson
import Data.Text qualified as Text
import Deriving.Aeson

data EventType
  = IdCardRegisteredEvent
  | IdCardScannedAtEntranceGateEvent
  | IdCardScannedAtExitGateEvent
  | FractionWasDroppedEvent
  | PriceWasCalculatedEvent
  | ExemptionWasGrantedEvent
  | DiscountWasBoughtEvent
  deriving stock (Eq, Enum, Bounded, Show, Read, Ord, Generic)

instance FromJSON EventType where
  parseJSON = genericParseJSON defaultOptions . handleEventPrefix
    where
      handleEventPrefix :: Value -> Value
      handleEventPrefix (String s) = String $ Text.append s "Event"
      handleEventPrefix x = x

instance ToJSON EventType where
  toJSON x = case x of
    IdCardRegisteredEvent -> String "IdCardRegistered"
    IdCardScannedAtEntranceGateEvent -> String "IdCardScannedAtEntranceGate"
    IdCardScannedAtExitGateEvent -> String "IdCardScannedAtExitGate"
    FractionWasDroppedEvent -> String "FractionWasDropped"
    PriceWasCalculatedEvent -> String "PriceWasCalculated"
    ExemptionWasGrantedEvent -> String "ExemptionWasGranted"
    DiscountWasBoughtEvent -> String "DiscountWasBought"
