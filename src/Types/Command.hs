module Types.Command where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
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

data Command = Command
  { commandId :: Text
  , createdAt :: UTCTime
  , cardId :: Text
  , commandType :: CommandType
  }
  deriving stock (Eq, Show, Read, Ord, Generic)

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    commandId <- o .: "command_id"
    createdAt <- o .: "created_at"
    commandType <- o .: "type"
    payload <- o .: "payload"
    cardId <- payload .: "card_id"

    pure Command{..}

instance ToJSON Command where
  toJSON Command{..} =
    object
      [ "command_id" .= commandId
      , "created_at" .= createdAt
      , "payload"
          .= object
            ["card_id" .= cardId]
      , "type" .= commandType
      ]

instance ParsableParam Command where
  parseParam = readEither
