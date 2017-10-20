{-# LANGUAGE OverloadedStrings #-}

module Templates (
    gameDescTemplate
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import qualified Data.Text.Lazy as TL

import Text.Numeral.Grammar (defaultInflection)
import qualified Text.Numeral.Language.ENG as ENG

pageTitle :: Int -> Html
pageTitle n = h1 $ toHtml $
  "The " <> fromMaybe plain fancy <> " Guy VOI Game" where
    plain = TL.pack $ show n
    fancy = (TL.toTitle . TL.fromStrict) <$> ENG.us_cardinal defaultInflection n

intro :: Html
intro = do
  p "God flips a coin..."

gameDescTemplate :: Int -> Html
gameDescTemplate stages = do
  pageTitle stages
  intro
  p $ do
    "a game with "
    toHtml $ show stages
    " stages"
