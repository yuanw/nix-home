{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import Data.Text qualified as T
import Data.Vector qualified as V
import Dhall (
    FromDhall,
    Generic,
    Text,
    Vector,
    auto,
    input,
 )

import System.Environment (getArgs, lookupEnv)
import Turtle hiding (input)
data Project = Project
    { name :: Text
    , repoUrl :: Text
    , checkoutCommand :: Maybe Text
    }
    deriving (Generic, Show)

instance FromDhall Project

work :: Vector Project -> Shell (Either Line Line)
work projects = do
  repo <- (inshell (V.foldr (\a b -> b <> " " <> (name a))  "gum choose " projects )  empty)
  inshellWithErr (format ("open -a firefox -g https://github.com/Workiva/"%s%"/") (lineToText repo)) empty

main :: IO ()
main = do
  projects <- loadProject
  view (work projects)


getConfigDir :: IO String
getConfigDir = liftA2 (++) base (pure "hi-chew")
  where
    base =
        fmap
            (maybe "$HOME/." (++ "/"))
            (lookupEnv "XDG_CONFIG_HOME")

loadProject :: IO (Vector Project)
loadProject = do
    file <- fmap (++  "/config.dhall") getConfigDir
    input auto (T.pack file)
