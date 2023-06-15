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
_currentTime :: TimeOfDay
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSetTime TimeOfDay
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  timeString = T.pack . show $ model ^. currentTime

  timeLabel = label (T.takeWhile (/= '.') timeString)
    `styleBasic` [textFont "Bold", textSize 80, textCenter, textMiddle, flexHeight 100]

  widgetTree = vstack [
      animFadeIn timeLabel `nodeKey` "fadeTimeLabel"
      ]

-- handleEvent
--   :: WidgetEnv AppModel AppEvent
--   -> WidgetNode AppModel AppEvent
--   -> AppModel
--   -> AppEvent
--   -> [AppEventResponse AppModel AppEvent]
-- handleEvent wenv node model evt = case evt of
--   AppInit -> []
--   AppIncrease -> [Model (model & clickCount +~ 1)]
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer timeOfDayProducer]
  AppSetTime time -> fadeInMsg time ++ [Model $ model & currentTime .~ time]
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

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  time <- getZonedTime
  return . localTimeOfDay . zonedTimeToLocalTime $ time

main :: IO ()
main = do
  time <- getLocalTimeOfDay
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      --how to bundle a png
      appWindowIcon "/Users/yuanwang/workspace/nix-home/hs-land/mono-stretchly/data/assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "/Users/yuanwang/workspace/nix-home/hs-land/mono-stretchly/data/assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model time = AppModel {
      _currentTime = time
                          }
