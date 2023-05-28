module Types.Payloads where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Time
import Deriving.Aeson
import GHC.Records
import Web.Twain
import Web.Twain.Types

data Payload
  = IdRegistered IdRegistrationPayload
  | IdScannedAtEntrance IdScannedAtEntrancePayload
  | IdScannedAtExit IdScannedAtExitPayload
  | FractionWasDropped FractionPayload
  | PriceCalculated PriceWasCalculatedPayload
  | DiscountWasBought DiscountWasBoughtPayload
  | ExemptionWasGranted ExemptionWasGrantedPayload
  deriving stock (Eq, Show, Read, Ord, Generic)

instance HasField "city" Payload (Maybe Text) where
  getField c =
    case c of
      IdRegistered p -> Just $ p.city
      _ -> Nothing

instance HasField "cardId" Payload Text where
  getField c =
    case c of
      (IdRegistered p) -> p.cardId
      (IdScannedAtEntrance p) -> p.cardId
      (IdScannedAtExit p) -> p.cardId
      (FractionWasDropped p) -> p.cardId
      (PriceCalculated p) -> p.cardId
      (DiscountWasBought p) -> p.cardId
      (ExemptionWasGranted p) -> p.cardId

instance ParsableParam Payload where
  parseParam = readEither

data IdRegistrationPayload = IdRegistrationPayload
  { address :: Text
  , cardId :: Text
  , city :: Text
  , personId :: Text
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] IdRegistrationPayload

data IdScannedAtEntrancePayload = IdScannedAtEntrancePayload
  { cardId :: Text
  , date :: Day
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] IdScannedAtEntrancePayload

data IdScannedAtExitPayload = IdScannedAtExitPayload
  { cardId :: Text
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] IdScannedAtExitPayload

data FractionPayload = FractionPayload
  { cardId :: Text
  , fractionType :: Text
  , weight :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] FractionPayload

data CalculatePricePayload = CalculatePricePayload
  { cardId :: Text
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] CalculatePricePayload

data PriceWasCalculatedPayload = PriceWasCalculatedPayload
  { cardId :: Text
  , priceAmount :: Float
  , priceCurrency :: Currency
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PriceWasCalculatedPayload

data DiscountWasBoughtPayload = DiscountWasBoughtPayload
  { cardId :: Text
  , discountPercentage :: Float
  , expiryDate :: Day
  , fractionType :: Text
  , weight :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] DiscountWasBoughtPayload

data ExemptionWasGrantedPayload = ExemptionWasGrantedPayload
  { cardId :: Text
  , expiryDate :: Day
  , fractionType :: Text
  , weight :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] ExemptionWasGrantedPayload

data Currency
  = EUR
  | LOL2
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving
    (ToJSON, FromJSON)
    via CustomJSON '[SumUntaggedValue] Currency
