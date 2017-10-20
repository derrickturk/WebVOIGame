{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))

import qualified Data.Text.Lazy as TL

gameDescTemplate :: Int -> Html
gameDescTemplate stages = do
  p $ toHtml $ "a game with " <> TL.pack (show stages) <> " stages"
