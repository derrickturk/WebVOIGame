{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Templates (
    gamePage
) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (MarkupM(Parent)) -- for svg "custom tag"

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

import Control.Monad (when)

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

svg :: MarkupM a -> MarkupM a
svg = Parent "svg" "<svg" "</svg>"

pageTitle :: Int -> T.Text
pageTitle n = "The " <> fromMaybe plain fancy <> " Guy VOI Game" where
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
    "A mysterious, wealthy, stranger flips a coin with a "
    slider $ SliderSetup { name = "successChance"
                         , value = 35.0
                         , min = 0.0
                         , max = 100.0
                         , step = 5.0
                         }
    "% chance of “heads”."
  p $ do
    "If he gets heads, he assigns a payout drawn from a log-normal \
      \distribution with a mean of "
    slider $ SliderSetup { name = "successMean"
                         , value = 500.0
                         , min = 100.0
                         , max = 2000.0
                         , step = 50.0
                         }
    " and a P10/P90 ratio of "
    slider $ SliderSetup { name = "successRatio"
                         , value = 5.0
                         , min = 1.5
                         , max = 10.0
                         , step = 0.5
                         }
    "."
  p $ do
    "The stranger offers to sell you the payout, if any, at a cost of "
    slider $ SliderSetup { name = "successCost"
                         , value = 200.0
                         , min = 0.0
                         , max = 1000.0
                         , step = 50.0
                         }
    " dollars."

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
stageBlocks n = go n guyNames guyRelations (1::Int) where
  go n _ _ _ 
    | n <= 0 = mempty
  go n (nm:names) (re:relns) i = do
    p $ do
      "The mysterious stranger's "
      text re
      ", "
      text nm
      ", approaches you and offers a deal: for a cost of "
      slider $ SliderSetup { name = "cost" <> T.pack (show i)
                           , value = 20.00
                           , min = 0.0
                           , max = 200.0
                           , step= 5.0
                           }
      " dollars , they will peek at his coin and tell you what they see \
        \before you have to make your decision. However, "
      text nm
      " is a known liar: in fact they tell the truth only "
      slider $ SliderSetup { name = "chance" <> T.pack (show i)
                           , value = 70.0
                           , min = 50.0
                           , max = 100.0
                           , step = 1.0
                           }
      "% of the time."
    go (n - 1) names relns (i + 1)
  go _ _ _ _ = error "internal error, names/relations/count out of sync"

resultsBlock :: Html
resultsBlock = do
  p "If you accept all the offers:"
  H.div ! A.id "results" $ do
    p $ do
      "On average, you will achieve a net payout of $"
      H.span ! A.id "resultMean" ! class_ "output-label" $ "0.00"
      "."
    p $ do
      "You'll lose money about "
      H.span ! A.id "resultChanceLoss" ! class_ "output-label" $ "0.00"
      "% of the time."
    p $ do
      "You'll lose a bunch of money about "
      H.span ! A.id "resultChanceBigLoss" ! class_ "output-label" $ "0.00"
      "% of the time."
    p $ do
      "You'll gain a bunch of money about "
      H.span ! A.id "resultChanceBigGain" ! class_ "output-label" $ "0.00"
      "% of the time."
    p "Here's the result distribution:"
    p $ svg ! A.id "resultHist" ! width "960" ! height "540" $ mempty

stageLinks :: Int -> Html
stageLinks n = H.div ! A.id "linkbar" $ do
  when (n > 0) $
    a ! class_ "prev-link"
      ! href (stringValue $ "game?stages=" <> show (n - 1))
      $ string ("<< " <> (show (n - 1)) <> "-stage game")
  a ! class_ "next-link"
    ! href (stringValue $ "game?stages=" <> show (n + 1))
    $ string ((show (n + 1)) <> "-stage game >>")

gamePageHead :: Int -> Html
gamePageHead stages = H.head $ do
  meta ! charset "UTF-8"
  H.title $ text $ pageTitle stages
  script
    ! type_ "text/javascript"
    ! src "https://d3js.org/d3.v4.min.js"
    $ mempty
  script
    ! type_ "text/javascript"
    ! src "js/game.js"
    $ mempty
  link
    ! rel "stylesheet"
    ! href "css/game.css"

gamePageBody :: Int -> Html
gamePageBody stages = do
  h1 $ text $ pageTitle stages
  intro
  stageBlocks stages
  resultsBlock
  stageLinks stages
  script ! type_ "text/javascript" $
    text $ "initGame(" <> T.pack (show stages) <> ");"

gamePage :: Int -> Html
gamePage stages = html $ do
  H.head $ gamePageHead stages
  body $ gamePageBody stages
