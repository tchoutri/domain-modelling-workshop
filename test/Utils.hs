module Utils
  ( -- * Random fixtures

    -- ** Random Payloads

  -- randomPayload
    RandomPayloadTemplate (..)
  , randomPayloadTemplate
  , randomIdRegistrationPayload
  , randomFractionWasDroppedPayload

    -- ** Random Command
  , randomCommand
  , RandomCommandTemplate (..)
  , randomCommandTemplate

    -- ** Random Event
  , randomEvent
  , RandomEventTemplate (..)
  , randomEventTemplate

    -- ** Individual Generators
  , genBody
  , genCommand
  , genEvent
  , genPayload
  , genEventType
  , genDay
  , genUTCTime
  , genFractionType
  , genCardId
  , genCity
  , randomIdScannedAtEntrancePayload
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time (Day, UTCTime (..), fromGregorian, secondsToDiffTime)
import GHC.Generics (Generic)
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as Range

import Types
import Types.Command
import Types.Event
import Types.EventType
import Types.Payloads

-- == Templates ==

data RandomCommandTemplate m = RandomCommandTemplate
  { commandId :: m Text
  , createdAt :: m UTCTime
  , cardId :: m Text
  , commandType :: m CommandType
  }
  deriving stock (Generic)

randomCommandTemplate :: (MonadIO m) => RandomCommandTemplate m
randomCommandTemplate =
  RandomCommandTemplate
    { commandId = H.sample genCommandId
    , createdAt = H.sample genUTCTime
    , cardId = H.sample genCardId
    , commandType = H.sample genCommandType
    }

randomCommand :: (MonadIO m) => RandomCommandTemplate m -> m Command
randomCommand
  RandomCommandTemplate
    { commandId = generateCommandId
    , createdAt = generateCreatedAt
    , cardId = generateCardId
    , commandType = generateCommandType
    } = do
    commandId <- generateCommandId
    createdAt <- generateCreatedAt
    cardId <- generateCardId
    commandType <- generateCommandType
    pure Command{..}

data RandomPayloadTemplate m = RandomPayloadTemplate
  { cardId :: m Text
  , address :: m Text
  , city :: m Text
  , personId :: m Text
  , date :: m Day
  , fractionType :: m Text
  , weight :: m Int
  , priceAmount :: m Float
  , priceCurrency :: m Currency
  }
  deriving stock (Generic)

randomPayloadTemplate :: (MonadIO m) => RandomPayloadTemplate m
randomPayloadTemplate =
  RandomPayloadTemplate
    { cardId = H.sample genCardId
    , address = H.sample genAddress
    , city = H.sample genCity
    , personId = H.sample genPersonId
    , date = H.sample genDay
    , fractionType = H.sample genFractionType
    , weight = H.sample genWeight
    , priceAmount = H.sample genPriceAmount
    , priceCurrency = pure EUR
    }

-- randomPayload :: (MonadIO m) => RandomPayloadTemplate m -> m Payload
-- randomPayload
--   RandomPayloadTemplate
--     { cardId = generateCardId
--     , address = generateAddress
--     , city = generateCity
--     , personId = generatePersonId
--     , date = generateDate
--     , fractionType = generateFractionType
--     , weight = generateWeight
--     , priceAmount = generatePriceAmount
--     , priceCurrency = generatePriceCurrency
--     } = do
--     cardId <- generateCardId
--     address <- generateAddress
--     city <- generateCity
--     personId <- generatePersonId
--     date <- generateDate
--     fractionType <- generateFractionType
--     weight <- generateWeight
--     priceAmount <- generatePriceAmount
--     priceCurrency <- generatePriceCurrency
--     pure Payload{..}

randomIdRegistrationPayload :: (MonadIO m) => RandomPayloadTemplate m -> m Payload
randomIdRegistrationPayload
  RandomPayloadTemplate
    { address = generateAddress
    , cardId = generateCardId
    , city = generateCity
    , personId = generatePersonId
    } = do
    address <- generateAddress
    cardId <- generateCardId
    city <- generateCity
    personId <- generatePersonId
    pure $ IdRegistered IdRegistrationPayload{..}

randomIdScannedAtEntrancePayload :: (MonadIO m) => RandomPayloadTemplate m -> m Payload
randomIdScannedAtEntrancePayload
  RandomPayloadTemplate
    { cardId = generateCardId
    , date = generateDate
    } = do
    cardId <- generateCardId
    date <- generateDate
    pure $ IdScannedAtEntrance IdScannedAtEntrancePayload{..}

randomFractionWasDroppedPayload :: (MonadIO m) => RandomPayloadTemplate m -> m Payload
randomFractionWasDroppedPayload
  RandomPayloadTemplate
    { cardId = generateCardId
    , fractionType = generateFractionType
    , weight = generateWeight
    } = do
    cardId <- generateCardId
    fractionType <- generateFractionType
    weight <- generateWeight
    pure $ FractionWasDropped FractionPayload{..}

randomEvent :: (MonadIO m) => RandomEventTemplate m -> m Event
randomEvent
  RandomEventTemplate
    { eventId = generateEventId
    , createdAt = generateCreatedAt
    , payload = generatePayload
    , eventType = generateEventType
    } = do
    eventId <- generateEventId
    createdAt <- generateCreatedAt
    payload <- generatePayload
    eventType <- generateEventType
    pure Event{..}

data RandomEventTemplate m = RandomEventTemplate
  { eventId :: m Text
  , createdAt :: m UTCTime
  , payload :: m Payload
  , eventType :: m EventType
  }

randomEventTemplate :: (MonadIO m) => RandomEventTemplate m
randomEventTemplate =
  RandomEventTemplate
    { eventId = H.sample genEventId
    , createdAt = H.sample genUTCTime
    , payload = H.sample genPayload
    , eventType = H.sample genEventType
    }

genEventId :: (MonadGen m) => m Text
genEventId = genCardId

-- == Random Generators ==

genBody :: (MonadGen m) => m Body
genBody = do
  command <- genCommand
  history <- H.list (Range.singleton 3) genEvent
  pure Body{..}

genEvent :: (MonadGen m) => m Event
genEvent = do
  eventId <- H.text (Range.constant 3 3) H.digit
  createdAt <- genUTCTime
  payload <- genPayload
  eventType <- genEventType
  pure Event{..}

genPayload :: (MonadGen m) => m Payload
genPayload =
  H.choice
    [ genIdRegisteredPayload
    ]

genIdRegisteredPayload :: (MonadGen m) => m Payload
genIdRegisteredPayload = do
  address <- genAddress
  cardId <- genCardId
  city <- genCity
  personId <- genPersonId
  pure $ IdRegistered IdRegistrationPayload{..}

genCommand :: (MonadGen m) => m Command
genCommand = do
  commandId <- genCommandId
  createdAt <- genUTCTime
  cardId <- genCardId
  commandType <- genCommandType
  pure Command{..}

genCommandId :: (MonadGen m) => m Text
genCommandId = H.text (Range.constant 3 3) H.digit

genCommandType :: (MonadGen m) => m CommandType
genCommandType = pure CalculatePrice

genPriceAmount :: (MonadGen m) => m Float
genPriceAmount = H.float (Range.constant 1 100)

genCardId :: (MonadGen m) => m Text
genCardId = H.text (Range.constant 1 25) H.alphaNum

genAddress :: (MonadGen m) => m Text
genAddress = H.text (Range.constant 1 25) H.ascii

genCity :: (MonadGen m) => m Text
genCity = H.text (Range.constant 1 10) H.ascii

genPersonId :: (MonadGen m) => m Text
genPersonId = H.text (Range.constant 3 3) H.digit

genEventType :: (MonadGen m) => m EventType
genEventType = H.enumBounded

genWeight :: (MonadGen m) => m Int
genWeight = H.int (Range.constant 1 100)

genUTCTime :: (MonadGen m) => m UTCTime
genUTCTime = do
  year <- toInteger <$> H.int (Range.constant 2000 2022)
  month <- H.int (Range.constant 1 12)
  day <- H.int (Range.constant 1 28)
  let date = fromGregorian year month day
  secs <- toInteger <$> H.int (Range.constant 0 86401)
  pure $ UTCTime date (secondsToDiffTime secs)

genDay :: (MonadGen m) => m Day
genDay = do
  year <- toInteger <$> H.int (Range.constant 2000 2022)
  month <- H.int (Range.constant 1 12)
  day <- H.int (Range.constant 1 28)
  pure $ fromGregorian year month day

genFractionType :: (MonadGen m) => m Text
genFractionType = H.element ["ConstructionWaste", "GreenWaste"]
