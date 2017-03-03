module Main (main) where

import System.Process
import System.Exit

main :: IO ExitCode
main = do
    rawSystem "./examples/test.sh" []
