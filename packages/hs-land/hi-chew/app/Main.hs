{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Dhall
  ( FromDhall,
    Generic,
    Text,
    Vector,
    auto,
    input,
  )
import System.Environment (getArgs, lookupEnv)
import Turtle hiding (input)

data Project = Project
  { name :: Text,
    repoUrl :: Text,
    checkoutCommand :: Maybe Text,
    devLog :: Text,
    prodLog :: Text
  }
  deriving (Generic, Show)

instance FromDhall Project

toMap :: Vector Project -> Map.Map Text Project
toMap = Map.fromList . map (\p -> (name p, p)) . V.toList

actions :: Project -> Text
actions project = "gum choose repo log" <> if (isJust . checkoutCommand) project then " checkout" else ""

openLink :: Text -> Shell (Either Line Line)
openLink = (`inshellWithErr` empty) . format ("open -a firefox -g " % s)

pasteCmd :: Text -> Shell (Either Line Line)
pasteCmd = (`inshellWithErr` empty) . format ("echo " % s % " | pbcopy")

openRepo :: Project -> Shell (Either Line Line)
openRepo project = openLink (repoUrl project)

openLog :: Project -> Shell (Either Line Line)
openLog project = do
  projectEnv <- inshell "gum choose dev prod" empty
  case lineToText projectEnv of
    "dev" -> openLink (devLog project)
    "prod" -> openLink (prodLog project)

work :: Map.Map Text Project -> Shell (Either Line Line)
work projectMap = do
  repo <- inshell (foldr (\a b -> b <> " " <> a) "gum choose " (Map.keys projectMap)) empty
  action <- inshell (actions . fromJust . flip Map.lookup projectMap . lineToText $ repo) empty
  case lineToText action of
    "repo" -> openRepo (fromJust . flip Map.lookup projectMap . lineToText $ repo)
    "log" -> openLog (fromJust . flip Map.lookup projectMap . lineToText $ repo)
    "checkout" ->
      pasteCmd
        ( fromJust
            . ( checkoutCommand
                  Control.Monad.<=< (flip Map.lookup projectMap . lineToText)
              )
            $ repo
        )

main :: IO ()
main = do
  projectMap <- fmap toMap loadProject
  view (work projectMap)

getConfigDir :: IO String
getConfigDir = liftA2 (++) base (pure "hi-chew")
  where
    base =
      fmap
        (maybe "$HOME/." (++ "/"))
        (lookupEnv "XDG_CONFIG_HOME")

loadProject :: IO (Vector Project)
loadProject = do
  file <- fmap (++ "/config.dhall") getConfigDir
  input auto (T.pack file)
