{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)

import Actions

server :: ScottyM ()
server = do
  get "/gameOutcome" gameAction
  get "/game" gamePageAction
  get "/js/game.js" $
    (setHeader "Content-Type" "text/javascript") >>
    (file "js/game.js")
  get "/css/game.css" $
    (setHeader "Content-Type" "text/css") >>
    (file "css/game.css")

main :: IO ()
main = do
  let opts = Options 0 $ setHost "127.0.0.1" $ setPort 3000 $ defaultSettings
  scottyOpts opts server
