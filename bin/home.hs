{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)

parser :: Parser (FilePath, FilePath)
parser = (,) <$> optPath "src"  's' "The source file"
             <*> optPath "dest" 'd' "The destination file"

main = do
    (src, dest) <- options "A simple `cp` utility" parser
    cp src dest
