module Main where

import FrkPs2.FRK2 qualified as FRK2
import System.IO qualified

main :: IO ()
main = FRK2.codeHandle 4096 System.IO.stdin System.IO.stdout
