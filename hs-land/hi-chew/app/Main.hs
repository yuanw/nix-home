{-# LANGUAGE OverloadedStrings #-}

module Main where


import Turtle

work = do
  repo <- (inshell "gum choose content-management-service bigsky" empty)
  inshellWithErr (format ("open -a firefox -g https://github.com/Workiva/"%s%"/") (lineToText repo)) empty

main :: IO ()
main = do
  view work
