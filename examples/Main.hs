module Main where

import TestMIME

       
main =
    testDeb "/var/lib/dpkg/status" >>= \(Right r) -> print (length r)