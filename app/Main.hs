{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State.Lazy (StateT, evalStateT)
import System.Random
import Control.Monad.Prob
import Data.VoiGame
import Web.Scotty
import qualified Data.Text.Lazy as TL

sampleSeed :: Int -> Prob a -> a
sampleSeed = sampleProbGen . mkStdGen

data TwoGuyVOIGame =
  TwoGuyVOIGame { successChance :: Chance
                , successMean :: Double
                , successP10P90 :: P10P90Ratio
                , successCost :: Double
                , firstGuyChance :: Chance
                , firstGuyCost :: Double
                , secondGuyChance :: Chance
                , secondGuyCost :: Double
                }

mkTwoGuyVOIGame :: (Double, Double, Double, Double,
                    Double, Double, Double, Double) -> Maybe TwoGuyVOIGame
mkTwoGuyVOIGame (sCh, sM, sR, sC, fgCh, fgC, sgCh, sgC) = do
  sCh' <- chance sCh
  sR' <- p10p90Ratio sR
  fgCh' <- chance fgCh
  sgCh' <- chance sgCh
  return $ TwoGuyVOIGame sCh' sM sR' sC fgCh' fgC sgCh' sgC

playTwoGuyVOIGame :: TwoGuyVOIGame -> StateT Double Prob Double
playTwoGuyVOIGame (TwoGuyVOIGame {..}) = voiGame
  (VoiSuccess successChance successMean successP10P90 successCost)
  [
    (VoiStage firstGuyChance firstGuyCost)
  , (VoiStage secondGuyChance secondGuyCost)
  ]

twoGuyGameAction :: ActionM ()
twoGuyGameAction = do
  seed <- param "seed"
  trls <- param "trials"

  sCh <- (/ 100.0) <$> param "successChance"
  sM <- param "successMean"
  sR <- param "successRatio"
  sC <- param "successCost"

  fgCh <- (/ 100.0) <$> param "firstGuyChance"
  fgC <- param "firstGuyCost"

  sgCh <- (/ 100.0) <$> param "secondGuyChance"
  sgC <- param "secondGuyCost"

  case mkTwoGuyVOIGame (sCh, sM, sR, sC, fgCh, fgC, sgCh, sgC) of
    Nothing -> html $ "<h1>You failed!</h1>"
    Just game -> do
      let mc = sampleSeed seed $ trials trls $ evalStateT (playTwoGuyVOIGame game) 0.0
      json mc

main :: IO ()
main = scotty 3000 $ do
  get "/twoguygame" twoGuyGameAction
  get "/game.html" $ file "html/game.html"
  get "/js/game.js" $ file "js/game.js"
