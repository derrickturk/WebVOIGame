{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Templates (
    gameDescTemplate
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import qualified Data.Text as T

import Text.Numeral.Grammar (defaultInflection)
import qualified Text.Numeral.Language.ENG as ENG

data SliderSetup =
  SliderSetup { name :: T.Text
              , value :: Double
              , min :: Double
              , max :: Double
              , step :: Double
              }

pageTitle :: Int -> Html
pageTitle n = h1 $ toHtml $
  "The " <> fromMaybe plain fancy <> " Guy VOI Game" where
    plain = T.pack $ show n
    fancy = T.toTitle <$> ENG.us_cardinal defaultInflection n

slider :: SliderSetup -> Html
slider SliderSetup { name, value, min, max, step } = do
  input
    ! type_ "range"
    ! A.id (textValue name)
    ! A.min (stringValue $ show min)
    ! A.max (stringValue $ show max)
    ! A.value (stringValue $ show value)
    ! A.step (stringValue $ show step)
  H.span
    ! A.id (textValue $ name <> "Label")
    ! class_ "input-label"
    $ (string $ show value)

intro :: Html
intro = do
  p $ do
    "God flips a coin with a "
    slider $ SliderSetup { name = "successChance"
                         , value = 30.0
                         , min = 0.0
                         , max = 100.0
                         , step = 5.0
                         }
    "% chance of “heads”."
  p $ do
    "If He gets heads, He assigns a payout drawn from a log-normal \
      \distribution with a mean of "
    slider $ SliderSetup { name = "successMean"
                         , value = 500.0
                         , min = 100.0
                         , max = 2000.0
                         , step = 50.0
                         }
    " and a P10/P90 ratio of "
    slider $ SliderSetup { name = "successRatio"
                         , value = 3.0
                         , min = 1.5
                         , max = 10.0
                         , step = 0.5
                         }
    "."
  p $ do
    "God offers to sell you the payout, if any, at a cost of "
    slider $ SliderSetup { name = "successCost"
                         , value = 200.0
                         , min = 0.0
                         , max = 1000.0
                         , step = 50.0
                         }
    "."

guyNames :: [T.Text]
guyNames = go names where
  go [] = (<> " Jr") <$> go names
  go (x:xs) = x:go xs
  names = [ "Bob"
          , "Sally"
          , "Joe"
          , "Mike"
          , "Cassandra"
          , "Katie"
          ]

guyRelations :: [T.Text]
guyRelations = relns <> cycle (("other " <>) <$> relns) where
  relns = [ "brother"
          , "niece"
          , "cousin"
          , "nephew"
          , "neighbor"
          , "co-worker"
          ]

stageBlocks :: Int -> Html
stageBlocks n = go n guyNames guyRelations 1 where
  go 0 _ _ _ = mempty
  go n (nm:names) (re:relns) i = do
    p $ do
      "God's "
      text re
      ", "
      text nm
      ", approaches you and offers a deal: for a cost of "
      slider $ SliderSetup { name = "cost" <> T.pack (show i)
                           , value = 10.00
                           , min = 0.0
                           , max = 200.0
                           , step= 5.0
                           }
      ", they will peek at God's coin and tell you what they see before you \
        \have to make your decision. However, "
      text nm
      " is a known liar: in fact they tell the truth only "
      slider $ SliderSetup { name = "chance" <> T.pack (show i)
                           , value = 60.0
                           , min = 0.0
                           , max = 100.0
                           , step = 1.0
                           }
      "% of the time."
    go (n - 1) names relns (i + 1)

gameDescTemplate :: Int -> Html
gameDescTemplate stages = do
  pageTitle stages
  intro
  stageBlocks stages
