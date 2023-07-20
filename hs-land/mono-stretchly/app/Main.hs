{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Monomer
import TextShow

import qualified Data.Text as T
import qualified Monomer.Lens as L

newtype AppModel = AppModel {
  _countDownSec :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | CountDown
  | AppDone
  deriving (Eq, Show)

makeLenses 'AppModel

-- https://mokehehe.hatenadiary.org/entry/20081204/time
-- miniBreak :: NominalDiffTime
-- miniBreak = secondsToNominalDiffTime 30

-- doneTime ::
-- https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-LocalTime.html#v:addLocalTime
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  -- timeString = (T.pack . show . $ model ^. countDownSec) + " seconds remaining"
  -- timeLabel = label (T.takeWhile (/= '.') timeString)
  --   `styleBasic` [textFont "Regular", textSize 80, textCenter, textMiddle, flexHeight 100]

  widgetTree = vstack [
      -- animFadeIn timeLabel `nodeKey` "fadeTimeLabel",
      label "Doing one time at a time" `styleBasic` [textCenter] ,
      spacer_ [width 5],
      label $ showt (model ^. countDownSec) <> " seconds remaing"  , --  `styleBasic` [textCenter],
      spacer_ [width 5],
      button "Skip this break" AppDone
    ]
-- https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/06-composite.md
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer countDownProducer]
  CountDown  ->  [if (model ^. countDownSec) > 0 then Model ( model & countDownSec -~ 1 ) else Request (ExitApplication True)]
  AppDone -> [Request (ExitApplication True )]


  -- where
  --   fadeInMsg time
  --     | truncate (todSec time) `mod` 10 /= 0 = []
  --     | otherwise = [Message "fadeTimeLabel" AnimationStart]

countDownProducer :: (AppEvent -> IO ()) -> IO ()
countDownProducer sendMsg = do
  sendMsg CountDown
  threadDelay $ 1000 * 1000
  countDownProducer sendMsg

-- getLocalTimeOfDay :: IO LocalTime
-- getLocalTimeOfDay = do
--   time <- getZonedTime
--   return . zonedTimeToLocalTime $ time

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Stretch",
      --how to bundle a png
      appWindowIcon "/Users/yuanwang/workspaces/nix-home/hs-land/mono-stretchly/data/assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "/Users/yuanwang/workspaces/nix-home/hs-land/mono-stretchly/data/assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _countDownSec = 20
    }
