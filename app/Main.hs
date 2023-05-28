module Main where

import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.Time qualified as Time
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Web.Twain

import Network.Wai.Middleware.RequestLogger qualified as Logger
import Price
import Types
import Types.Command
import Types.Event
import Types.EventType
import Types.Payloads
import Validation

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Starting server on http://localhost:" <> show port
  let app = Logger.logStdoutDev $ foldr ($) (notFound missing) routes
  run port app

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
                Event
                  "1"
                  currentTime
                  (PriceCalculated $ PriceWasCalculatedPayload{cardId = body.command.cardId, priceAmount = price, priceCurrency = EUR})
                  PriceWasCalculatedEvent
          liftIO $ print (toJSON response)
          send $ json response

missing :: ResponderM a
missing = send $ html "Not found..."
