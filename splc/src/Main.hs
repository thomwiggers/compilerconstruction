module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))

data InputSrc = StdInput | Filename String

data ArgOptions = ArgOptions {
    pretty_print :: Bool,
    input :: InputSrc
    }

argOptions :: Parser ArgOptions
argOptions = ArgOptions
    <$> prettyPrintSwitch
    <*> (parseFile <|> parseStdIn)

prettyPrintSwitch :: Parser Bool
prettyPrintSwitch = switch $
       long "pretty-print"
    <> short 'p'
    <> help "run the pretty printer"

parseFile :: Parser InputSrc
parseFile = Filename <$> strOption (
      metavar "FILE"
   <> help "filename")

parseStdIn :: Parser InputSrc
parseStdIn = flag' StdInput (short 'i' <> long "stdin")

main :: IO ()
main = do
    opts <- execParser theOpts
    print "hello world"
    where
        theOpts = info (argOptions <**> helper) $
               fullDesc 
            <> progDesc "SPL compiler, pretty-printer and parser"
