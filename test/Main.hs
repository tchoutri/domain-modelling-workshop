{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Main (main) where

import Data.Aeson
import Data.Aeson.QQ
import Data.Maybe
import Optics.Core
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple

import Price (computePrice)
import Types
import Types.Command
import Types.Event
import Types.EventType
import Types.Payloads
import Utils
import Validation

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup
    "Tests"
    [ testGroup
        "Command parsing"
        [ testCase "Parse CommandType" testParseCommandType
        , testCase "Serialise CommandType" testSerialiseCommandType
        , testCase "Parse Command" testParseCommand
        ]
    , testGroup
        "Body"
        [ testCase "Body parsing" testParseBody
        , testCase "Body validation succesful" testBodyValidation
        , testCase "Body validation fails" testBodyValidationFailure
        ]
    , testGroup
        "History"
        [ testCase "Serialising Response event" testSerialiseResponseEvent
        ]
    , testGroup
        "Pricing"
        [ testCase "Price with no fraction is 0" testPriceZero
        , testCase "Construction waste from South Park" testConstructionWasteSouthPark
        ]
    ]

testParseCommand :: IO ()
testParseCommand = do
  let input1 = "{\r\n    \"command_id\": \"9fa9f790-7035-4542-ac9f-24a28a857bbc\",\r\n    \"created_at\": \"2023-05-26T11:29:21.339243Z\",\r\n    \"payload\": {\r\n      \"card_id\": \"123\"\r\n    },\r\n    \"type\": \"CalculatePrice\"\r\n  }"
  let result1 = eitherDecodeStrict' input1 :: Either String Command
  assertEqual
    "Command is parsed properly"
    ( Right
        ( Command
            { commandId = "9fa9f790-7035-4542-ac9f-24a28a857bbc"
            , createdAt = read "2023-05-26 11:29:21.339243 UTC"
            , cardId = "123"
            , commandType = CalculatePrice
            }
        )
    )
    result1

testParseCommandType :: IO ()
testParseCommandType = do
  let input1 = "\"CalculatePrice\""
  let result1 = eitherDecodeStrict' input1 :: Either String CommandType
  assertEqual
    "CommandType is parsed properly"
    (Right CalculatePrice)
    result1

testSerialiseCommandType :: IO ()
testSerialiseCommandType = do
  assertEqual
    "CommandType is serialised to JSON properly"
    (encode CalculatePrice)
    "\"CalculatePrice\""

testParseBody :: IO ()
testParseBody = do
  let input1 = "\r\n\r\n{\r\n  \"command\": {\r\n    \"command_id\": \"9fa9f790-7035-4542-ac9f-24a28a857bbc\",\r\n    \"created_at\": \"2023-05-26T11:29:21.339243Z\",\r\n    \"payload\": {\r\n      \"card_id\": \"123\"\r\n    },\r\n    \"type\": \"CalculatePrice\"\r\n  },\r\n  \"history\": [\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339229Z\",\r\n      \"event_id\": \"1bd45275-1711-4ab5-8d6c-caee2ab14d45\",\r\n      \"payload\": {\r\n        \"address\": \"Point Dume\",\r\n        \"card_id\": \"123\",\r\n        \"city\": \"Malibu\",\r\n        \"person_id\": \"Tony Stark\"\r\n      },\r\n      \"type\": \"IdCardRegistered\"\r\n    },\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339234Z\",\r\n      \"event_id\": \"b06974f4-10e9-4a2b-a465-69063ebf0eb6\",\r\n      \"payload\": {\r\n        \"card_id\": \"123\",\r\n        \"date\": \"2023-02-10\"\r\n      },\r\n      \"type\": \"IdCardScannedAtEntranceGate\"\r\n    },\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339238Z\",\r\n      \"event_id\": \"40e0ebd8-d471-4198-aaa6-8bacdbaf2cf7\",\r\n      \"payload\": {\r\n        \"card_id\": \"123\"\r\n      },\r\n      \"type\": \"IdCardScannedAtExitGate\"\r\n    }\r\n  ]\r\n}"
  let result1 = eitherDecodeStrict' input1 :: Either String Body
  assertEqual
    "Body is parsed properly"
    ( Right
        ( Body
            { command =
                Command
                  { commandId = "9fa9f790-7035-4542-ac9f-24a28a857bbc"
                  , createdAt = read "2023-05-26 11:29:21.339243 UTC"
                  , cardId = "123"
                  , commandType = CalculatePrice
                  }
            , history =
                [ Event
                    { eventId = "1bd45275-1711-4ab5-8d6c-caee2ab14d45"
                    , createdAt = read "2023-05-26 11:29:21.339229 UTC"
                    , payload =
                        IdRegistered
                          IdRegistrationPayload
                            { cardId = "123"
                            , address = "Point Dume"
                            , city = "Malibu"
                            , personId = "Tony Stark"
                            }
                    , eventType = IdCardRegisteredEvent
                    }
                , Event
                    { eventId = "b06974f4-10e9-4a2b-a465-69063ebf0eb6"
                    , createdAt = read "2023-05-26 11:29:21.339234 UTC"
                    , payload =
                        IdScannedAtEntrance
                          IdScannedAtEntrancePayload
                            { cardId = "123"
                            , date = read "2023-02-10"
                            }
                    , eventType = IdCardScannedAtEntranceGateEvent
                    }
                , Event
                    { eventId = "40e0ebd8-d471-4198-aaa6-8bacdbaf2cf7"
                    , createdAt = read "2023-05-26 11:29:21.339238 UTC"
                    , payload =
                        IdScannedAtExit
                          IdScannedAtExitPayload
                            { cardId = "123"
                            }
                    , eventType = IdCardScannedAtExitGateEvent
                    }
                ]
            }
        )
    )
    result1

testBodyValidation :: IO ()
testBodyValidation = do
  let input1 = "\r\n\r\n{\r\n  \"command\": {\r\n    \"command_id\": \"9fa9f790-7035-4542-ac9f-24a28a857bbc\",\r\n    \"created_at\": \"2023-05-26T11:29:21.339243Z\",\r\n    \"payload\": {\r\n      \"card_id\": \"123\"\r\n    },\r\n    \"type\": \"CalculatePrice\"\r\n  },\r\n  \"history\": [\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339229Z\",\r\n      \"event_id\": \"1bd45275-1711-4ab5-8d6c-caee2ab14d45\",\r\n      \"payload\": {\r\n        \"address\": \"Point Dume\",\r\n        \"card_id\": \"123\",\r\n        \"city\": \"Malibu\",\r\n        \"person_id\": \"Tony Stark\"\r\n      },\r\n      \"type\": \"IdCardRegistered\"\r\n    },\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339234Z\",\r\n      \"event_id\": \"b06974f4-10e9-4a2b-a465-69063ebf0eb6\",\r\n      \"payload\": {\r\n        \"card_id\": \"123\",\r\n        \"date\": \"2023-02-10\"\r\n      },\r\n      \"type\": \"IdCardScannedAtEntranceGate\"\r\n    },\r\n    {\r\n      \"created_at\": \"2023-05-26T11:29:21.339238Z\",\r\n      \"event_id\": \"40e0ebd8-d471-4198-aaa6-8bacdbaf2cf7\",\r\n      \"payload\": {\r\n        \"card_id\": \"123\"\r\n      },\r\n      \"type\": \"IdCardScannedAtExitGate\"\r\n    }\r\n  ]\r\n}"
  let Right body = eitherDecodeStrict' input1 :: Either String Body
  assertBool
    "Body is validated"
    (isJust $ validate body)

testBodyValidationFailure :: IO ()
testBodyValidationFailure = do
  command <- randomCommand randomCommandTemplate
  let history = []
  let body = Body{..}
  assertBool
    "Body is not validated"
    (isNothing $ validate body)

testPriceZero :: IO ()
testPriceZero = do
  payload1 <-
    randomIdRegistrationPayload randomPayloadTemplate

  event1 <-
    randomEvent $
      randomEventTemplate
        { eventType = pure IdCardRegisteredEvent
        , payload = pure payload1
        }

  let payload2 =
        randomIdScannedAtEntrancePayload $
          randomPayloadTemplate
            { cardId = pure payload1.cardId
            }

  event2 <-
    randomEvent $
      randomEventTemplate
        { eventType = pure IdCardScannedAtEntranceGateEvent
        , payload = payload2
        }

  assertEqual
    "Price is 0.0"
    0.0
    (computePrice [event1, event2])

testConstructionWasteSouthPark :: IO ()
testConstructionWasteSouthPark = do
  payload1 <-
    randomIdRegistrationPayload (randomPayloadTemplate & #city .~ pure "South Park")

  event1 <-
    randomEvent $
      randomEventTemplate
        { eventType = pure IdCardRegisteredEvent
        , payload = pure payload1
        }

  let payload2 =
        randomIdScannedAtEntrancePayload $
          randomPayloadTemplate
            { cardId = pure payload1.cardId
            }

  event2 <-
    randomEvent $
      randomEventTemplate
        { eventType = pure IdCardScannedAtEntranceGateEvent
        , payload = payload2
        }

  payload3 <-
    randomFractionWasDroppedPayload $
      randomPayloadTemplate
        & #city .~ pure "South Park"
        & #weight .~ pure 85
        & #fractionType .~ pure "Construction waste"

  event3 <-
    randomEvent
      randomEventTemplate
        { eventType = pure FractionWasDroppedEvent
        , payload = pure payload3
        }

  let events =
        [ event1
        , event2
        , event3
        ]
  pPrint events
  assertEqual
    "South Park Construction waste price"
    0.00
    (computePrice events)

testSerialiseResponseEvent :: IO ()
testSerialiseResponseEvent = do
  let expected =
        [aesonQQ|
          {
            "created_at": "2023-05-28T12:05:56Z",
            "event_id": "91e42e60-784f-4c9c-b842-581512ed950b",
            "payload": {
              "card_id": "123",
              "price_amount": 10.5,
              "price_currency": "EUR"
            },
            "type": "PriceWasCalculated"
          }
        |]

  let actual =
        toJSON $
          Event
            "91e42e60-784f-4c9c-b842-581512ed950b"
            (read "2023-05-28 12:05:56Z")
            (PriceCalculated PriceWasCalculatedPayload{cardId = "123", priceAmount = 10.5, priceCurrency = EUR})
            PriceWasCalculatedEvent
  assertEqual
    "\"Price was calculated\" event is serialised properly"
    expected
    actual
