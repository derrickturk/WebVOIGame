{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import Control.Monad (forM)
import Control.Monad.Trans.State.Lazy (evalStateT)

import Control.Monad.Prob
import Data.VoiGame

import Web.Scotty
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Network.HTTP.Types.Status (unprocessableEntity422)
import qualified Data.Text.Lazy as TL

import Control.Concurrent (forkIO, threadDelay)
import System.Process (createProcess, CreateProcess(..), shell)

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
    ch <- (/ 100.0) <$> (param $ "chance" `mappend` (TL.pack $ show i))
    cost <- param $ "cost" `mappend` (TL.pack $ show i)
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

server :: ScottyM ()
server = do
  get "/gameOutcome" gameAction
  get "/game.html" $
    (setHeader "Content-Type" "text/html") >>
    (file "html/game.html")
  get "/js/game.js" $
    (setHeader "Content-Type" "text/javascript") >>
    (file "js/game.js")
  get "/css/game.css" $
    (setHeader "Content-Type" "text/css") >>
    (file "css/game.css")

main :: IO ()
main = do
  _ <- forkIO $ do
    threadDelay 10000 -- 10ms
    _ <- createProcess $
      (shell "start http://localhost:3000/game.html") { detach_console = True }
    return ()
  let opts = Options 0 $ setHost "127.0.0.1" $ setPort 3000 $ defaultSettings
  scottyOpts opts server
