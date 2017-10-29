{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Actions (
    gameAction
  , gamePageAction
) where

import System.Random
import Control.Monad (forM)
import Control.Monad.Trans.State.Lazy (evalStateT)
import Data.Monoid ((<>))

import Control.Monad.Prob
import Data.VoiGame
import qualified Data.Text.Lazy as TL

import Network.HTTP.Types.Status (unprocessableEntity422)

import Web.Scotty

import Text.Blaze.Html.Renderer.Text (renderHtml)

import Templates

sampleSeed :: Int -> Prob a -> a
sampleSeed = sampleProbGen . mkStdGen

makeSuccess :: Double -> Double -> Double -> Double -> Maybe VoiSuccess
makeSuccess sChance sMean sP10P90 sCost = do
  sChance' <- chance sChance
  sP10P90' <- p10p90Ratio sP10P90
  return $ VoiSuccess sChance' sMean sP10P90' sCost

makeStage :: Double -> Double -> Maybe VoiStage
makeStage sChance sCost = do
  sChance' <- chance sChance
  return $ VoiStage sChance' sCost

gameFromParams :: ActionM (Maybe VoiGame)
gameFromParams = do
  sCh <- (/ 100.0) <$> param "successChance"
  sM <- param "successMean"
  sR <- param "successRatio"
  sC <- param "successCost"
  let success = makeSuccess sCh sM sR sC

  stageCount <- param "stages" :: ActionM Int
  stages <- forM (enumFromTo 1 stageCount) $ \i -> do
    ch <- (/ 100.0) <$> (param $ "chance" <> (TL.pack $ show i))
    cost <- param $ "cost" <> (TL.pack $ show i)
    return $ makeStage ch cost

  return $ do
    success' <- success
    stages' <- sequence stages
    return $ VoiGame success' stages'

gameAction :: ActionM ()
gameAction = do
  seed <- param "seed"
  trls <- param "trials"

  game <- gameFromParams

  case game of
    Nothing -> do
      status unprocessableEntity422
      text "Invalid parameter."
    Just g ->
      json $ sampleSeed seed $ trials trls $ evalStateT (playVoiGame g) 0.0

gamePageAction :: ActionM ()
gamePageAction = do
  stageCount <- param "stages" `rescue` const (return 0) :: ActionM Int
  html $ renderHtml $ gamePage stageCount
