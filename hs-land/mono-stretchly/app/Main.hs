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
  _currentTime :: LocalTime
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSetTime LocalTime
  | AppDone
  deriving (Eq, Show)

makeLenses 'AppModel

-- https://mokehehe.hatenadiary.org/entry/20081204/time
miniBreak :: NominalDiffTime
miniBreak = secondsToNominalDiffTime 30

-- doneTime ::
-- https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-LocalTime.html#v:addLocalTime
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  timeString = T.pack . show . localTimeOfDay $ model ^. currentTime
  timeLabel = label (T.takeWhile (/= '.') timeString)
    `styleBasic` [textFont "Regular", textSize 80, textCenter, textMiddle, flexHeight 100]

  widgetTree = vstack [
      animFadeIn timeLabel `nodeKey` "fadeTimeLabel"
    ]
-- https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/06-composite.md
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer timeOfDayProducer]
  AppSetTime time -> fadeInMsg ( localTimeOfDay time) ++ [Model $ model & currentTime .~ time]
  AppDone -> [Request (ExitApplication True )]


  where
    fadeInMsg time
      | truncate (todSec time) `mod` 10 /= 0 = []
      | otherwise = [Message "fadeTimeLabel" AnimationStart]

timeOfDayProducer :: (AppEvent -> IO ()) -> IO ()
timeOfDayProducer sendMsg = do
  time <- getLocalTimeOfDay
  sendMsg (AppSetTime time)
  threadDelay $ 1000 * 1000
  timeOfDayProducer sendMsg

getLocalTimeOfDay :: IO LocalTime
getLocalTimeOfDay = do
  time <- getZonedTime
  return . zonedTimeToLocalTime $ time

main :: IO ()
main = do
  time <- getLocalTimeOfDay
  startApp (model time) handleEvent buildUI config
  where
    config = [
      appWindowTitle "Producers",
      --how to bundle a png
      appWindowIcon "/Users/yuanwang/workspace/nix-home/hs-land/mono-stretchly/data/assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "/Users/yuanwang/workspace/nix-home/hs-land/mono-stretchly/data/assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model time = AppModel {
      _currentTime = time
    }
