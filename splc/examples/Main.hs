module Main (main) where

import System.Process
import System.Exit

main :: IO ExitCode
main = do
    code <- system "./examples/test.sh"
    exitWith code
