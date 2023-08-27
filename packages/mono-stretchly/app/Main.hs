{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Monomer
import qualified Monomer.Lens as L
import TextShow

newtype AppModel = AppModel
  { _countDownSec :: Int
  }
  deriving (Eq, Show)

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
buildUI ::
  WidgetEnv AppModel AppEvent ->
  AppModel ->
  WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree
  where
    -- timeString = (T.pack . show . $ model ^. countDownSec) + " seconds remaining"
    -- timeLabel = label (T.takeWhile (/= '.') timeString)
    --   `styleBasic` [textFont "Regular", textSize 80, textCenter, textMiddle, flexHeight 100]

    widgetTree =
      vstack
        [ -- animFadeIn timeLabel `nodeKey` "fadeTimeLabel",

          label " " `styleBasic` [textCenter],
          label " * Doing one time at a time" `styleBasic` [textCenter],
          -- label " * Communicate intent precisely" `styleBasic` [textCenter],
          -- label " * Edge cases matter" `styleBasic` [textCenter],
          -- label " * Favor reading code over writing code." `styleBasic` [textCenter],
          -- label " * Only one obvious way to do things" `styleBasic` [textCenter],
          -- label " * Runtime crashes are better than bugs." `styleBasic` [textCenter],
          -- label " * Compile errors are better than runtime crashes." `styleBasic` [textCenter],
          -- label " * Incremental improvements." `styleBasic` [textCenter],
          -- label " * Avoid local maximums." `styleBasic` [textCenter],
          -- label " * Reduce the amount one must remember." `styleBasic` [textCenter],
          -- label " * Focus on code rather than style." `styleBasic` [textCenter],
          -- label " * Resource allocation may fail; resource deallocation must succeed." `styleBasic` [textCenter],
          -- label " * Memory is a resource." `styleBasic` [textCenter],
          -- label " * Together we serve the users." `styleBasic` [textCenter],
          spacer_ [width 5],
          label (showt (model ^. countDownSec) <> " seconds remaing") `styleBasic` [textCenter],
          spacer_ [width 5],
          button "Skip this break" AppDone
        ]

-- https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/06-composite.md
handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer countDownProducer]
  CountDown -> [if (model ^. countDownSec) > 0 then Model (model & countDownSec -~ 1) else Request (ExitApplication True)]
  AppDone -> [Request (ExitApplication True)]

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
    config =
      [ appWindowTitle "Let's stretch",
        -- how to bundle a png
        appWindowIcon "/Users/yuanwang/workspaces/nix-home/packages/mono-stretchly/data/assets/images/icon.png",
        appTheme darkTheme,
        appFontDef "Regular" "/Users/yuanwang/workspaces/nix-home/packages/mono-stretchly/data/assets/fonts/Roboto-Regular.ttf",
        appInitEvent AppInit
      ]
    model =
      AppModel
        { _countDownSec = 20
        }
