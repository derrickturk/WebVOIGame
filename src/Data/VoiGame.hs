module Data.VoiGame (
    Chance
  , chance
  , getChance
  , P10P90Ratio
  , p10p90Ratio
  , getP10P90Ratio
  , VoiSuccess(..)
  , VoiStage(..)
  , voiGame
) where

import Control.Monad (when)
import Control.Monad.Prob
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

newtype Chance = Chance { getChance :: Double }

chance :: Double -> Maybe Chance
chance x
  | x < 0 || x > 1 = Nothing
  | otherwise = Just $ Chance x

newtype P10P90Ratio = P10P90Ratio { getP10P90Ratio :: Double }

p10p90Ratio :: Double -> Maybe P10P90Ratio
p10p90Ratio r
  | r <= 1 = Nothing
  | otherwise = Just $ P10P90Ratio r

data VoiSuccess = VoiSuccess { successChance :: !Chance
                             , successMean :: !Double
                             , successP10P90 :: !P10P90Ratio
                             , successCost :: !Double
                             }

data VoiStage = VoiStage { stageChance :: !Chance
                         , stageCost :: !Double
                         }

voiGame :: VoiSuccess -> [VoiStage] -> StateT Double Prob Double
voiGame success stages = do
  actualSuccess <- lift $ binary $ getChance $ successChance success
  doIt <- runStages actualSuccess stages
  when doIt $ modify (subtract $ successCost success)
  when (doIt && actualSuccess) $ do
    payout <- lift $ lognormalMeanRatio
      (successMean success) (getP10P90Ratio $ successP10P90 success)
    modify (+ payout)
  get

runStages :: Bool -> [VoiStage] -> StateT Double Prob Bool
runStages _ [] = return True
runStages actualSuccess (s:stages) = do
  modify (subtract $ stageCost s)
  correctMeasurement <- lift $ binary $ getChance $ stageChance s
  if correctMeasurement && not actualSuccess
    then return False
    else runStages actualSuccess stages
