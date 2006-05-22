module Main where

import System.Environment

import Base64

main = 
    do [fp] <- getArgs
       readFile fp >>= putStr . decode . encode

