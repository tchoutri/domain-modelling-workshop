module Main where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Time qualified as Time
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Web.Twain

import Price
import Types
import Validation

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Starting server on http://localhost:" <> show port
  run port $
    foldr ($) (notFound missing) routes

routes :: [Middleware]
routes =
  [ get "/validate" validateHandler
  , post "/handle-command" handleCommand
  ]

validateHandler :: ResponderM a
validateHandler = send $ raw status200 [] "OK"

handleCommand :: ResponderM ()
handleCommand = do
  stuff :: Value <- fromBody
  let result :: Result Body = fromJSON stuff
  case result of
    Error _ -> pure ()
    Success b -> do
      case validate b of
        Nothing -> pure ()
        Just body -> do
          let price = computePrice body.history
          currentTime <- liftIO Time.getCurrentTime
          let response =
                Event "1" currentTime (defaultPayload{cardId = "123", priceAmount = Just price, priceCurrency = Just EUR}) PriceWasCalculated
          liftIO $ print (encode response)
          send $ json $ response

missing :: ResponderM a
missing = send $ html "Not found..."
