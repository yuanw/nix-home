{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import Data.Text qualified as T
import Data.Vector qualified as V
import qualified Data.Map as Map
import Dhall (
    FromDhall,
    Generic,
    Text,
    Vector,
    auto,
    input,
 )
import Data.Maybe (fromJust)
import System.Environment (getArgs, lookupEnv)
import Turtle hiding (input)

data Project = Project
    { name :: Text
    , repoUrl :: Text
    , checkoutCommand :: Maybe Text
    }
    deriving (Generic, Show)

instance FromDhall Project

toMap :: Vector Project -> Map.Map Text Project
toMap = Map.fromList .  map (\p -> (name p, p))  . V.toList

work :: Map.Map Text Project -> Shell (Either Line Line)
work projectMap = do
  repo <- (inshell ( foldr (\a b -> b <> " " <>  a)  "gum choose " (Map.keys projectMap ) ) empty)
  inshellWithErr (format ("open -a firefox -g "%s)  ( repoUrl . fromJust . flip Map.lookup projectMap . lineToText $ repo)) empty

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
    file <- fmap (++  "/config.dhall") getConfigDir
    input auto (T.pack file)
