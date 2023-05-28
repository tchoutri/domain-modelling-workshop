module Price where

import Data.List qualified as List
import Effectful
import Effectful.State.Static.Local (State, evalState, get)

import Debug.Trace
import PricingState
import Types.Event
import Types.EventType
import Types.Payloads

computePrice :: [Event] -> Float
computePrice history = runPureEff . evalState defaultPricingState $ do
  setParameters history
  case List.filter (\e -> e.eventType == FractionWasDroppedEvent) history of
    [] -> pure 0.0
    events -> do
      prices <- traverse (\e -> computePriceForFraction e.payload) events
      traceShowM prices
      pure $ round' 2 $ sum prices

computePriceForFraction :: Payload -> Eff '[State PricingState] Float
computePriceForFraction (FractionWasDropped payload)
  | payload.fractionType == "Construction waste" = computeConstructionWastePrice payload
  | payload.fractionType == "Green waste" = computeGreenWastePrice payload
  | otherwise = pure 0
computePriceForFraction _ = pure 0.0

round' :: Integer -> Float -> Float
round' sg num = (fromIntegral . round $ num * f) / f
  where
    f = 10 ^ sg

computeConstructionWastePrice :: FractionPayload -> Eff '[State PricingState] Float
computeConstructionWastePrice payload = do
  PricingState{constructionWastePrice, constructionWasteExemption} <- get
  traceShowM constructionWastePrice
  traceShowM constructionWasteExemption
  pure $ constructionWastePrice * (fromIntegral @Int @Float $ computeWeight payload.weight constructionWasteExemption)

computeGreenWastePrice :: FractionPayload -> Eff '[State PricingState] Float
computeGreenWastePrice payload = do
  PricingState{greenWastePrice, greenWasteExemption} <- get
  traceShowM greenWastePrice
  traceShowM greenWasteExemption
  pure $ greenWastePrice * (fromIntegral @Int @Float $ computeWeight payload.weight greenWasteExemption)

computeWeight
  :: Int
  -- ^ Initial weight
  -> Int
  -- ^ Exemption
  -> Int
computeWeight initial exemption
  | initial - exemption <= 0 = 0
  | otherwise = initial - exemption
