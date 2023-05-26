module Main (main) where

import Data.Aeson
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import Price
import Types
import Validation

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec =
  testGroup
    "Tests"
    [ testGroup
        "Payload parsing"
        [ testCase "Simple Payload parses correctly" testParseSimplePayload
        , testCase "Full payload parses correctly" testParseFullPayload
        ]
    , testGroup
        "Command parsing"
        [ testCase "Parse CommandType" testParseCommandType
        , testCase "Serialise CommandType" testSerialiseCommandType
        , testCase "Parse Command" testParseCommand
        ]
    , testGroup
        "Body"
        [ testCase "Body parsing" testParseBody
        , testCase "Body validation" testBodyValidation
        ]
    , testGroup
        "History"
        [ testCase "Price with no fraction is 0" testPriceZero
        ]
    ]

testParseSimplePayload :: IO ()
testParseSimplePayload = do
  let input1 = "{ \"card_id\": \"123\" }"
  let result1 = eitherDecodeStrict' input1 :: Either String Payload
  assertEqual
    "Payload is parsed properly"
    (Right $ Payload "123" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    result1

testParseFullPayload :: IO ()
testParseFullPayload = do
  let input2 = "{\"address\": \"Point Dume\",\"card_id\": \"123\",\"city\": \"Malibu\",\"person_id\": \"Tony Stark\"}"
  let result2 = eitherDecodeStrict' input2 :: Either String Payload
  assertEqual
    "Payload is parsed properly"
    (Right $ Payload "123" (Just "Point Dume") (Just "Malibu") (Just "Tony Stark") Nothing Nothing Nothing Nothing Nothing)
    result2

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
            , payload =
                Payload
                  { cardId = "123"
                  , address = Nothing
                  , city = Nothing
                  , personId = Nothing
                  , date = Nothing
                  , fractionType = Nothing
                  , weight = Nothing
                  , priceAmount = Nothing
                  , priceCurrency = Nothing
                  }
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
                  , payload = Payload{cardId = "123", address = Nothing, city = Nothing, personId = Nothing, date = Nothing, fractionType = Nothing, weight = Nothing, priceAmount = Nothing, priceCurrency = Nothing}
                  , commandType = CalculatePrice
                  }
            , history =
                [ Event
                    { eventId = "1bd45275-1711-4ab5-8d6c-caee2ab14d45"
                    , createdAt = read "2023-05-26 11:29:21.339229 UTC"
                    , payload =
                        Payload
                          { cardId = "123"
                          , address = Just "Point Dume"
                          , city = Just "Malibu"
                          , personId = Just "Tony Stark"
                          , date = Nothing
                          , fractionType = Nothing
                          , weight = Nothing
                          , priceAmount = Nothing
                          , priceCurrency = Nothing
                          }
                    , eventType = IdCardRegistered
                    }
                , Event
                    { eventId = "b06974f4-10e9-4a2b-a465-69063ebf0eb6"
                    , createdAt = read "2023-05-26 11:29:21.339234 UTC"
                    , payload =
                        Payload
                          { cardId = "123"
                          , address = Nothing
                          , city = Nothing
                          , personId = Nothing
                          , date = Just (read "2023-02-10")
                          , fractionType = Nothing
                          , weight = Nothing
                          , priceAmount = Nothing
                          , priceCurrency = Nothing
                          }
                    , eventType = IdCardScannedAtEntranceGate
                    }
                , Event
                    { eventId = "40e0ebd8-d471-4198-aaa6-8bacdbaf2cf7"
                    , createdAt = read "2023-05-26 11:29:21.339238 UTC"
                    , payload =
                        Payload
                          { cardId = "123"
                          , address = Nothing
                          , city = Nothing
                          , personId = Nothing
                          , date = Nothing
                          , fractionType = Nothing
                          , weight = Nothing
                          , priceAmount = Nothing
                          , priceCurrency = Nothing
                          }
                    , eventType = IdCardScannedAtExitGate
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

testPriceZero :: IO ()
testPriceZero =
  assertEqual
    "Price is 0.0"
    0.0
    (computePrice [])
