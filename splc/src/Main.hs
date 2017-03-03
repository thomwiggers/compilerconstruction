module Main (main) where

import Options.Applicative (subparser, Parser, info, execParser, fullDesc,
        helper, progDesc, (<**>), command, (<|>), argument, str, help,
        flag', short, long, metavar)
import Data.Semigroup ((<>))

import Text.Megaparsec (runParser, parseErrorPretty)
import SplParser (spl)
import SplPrettyPrinter
import System.Exit

data InputSrc = StdInput | Filename String
    deriving (Eq, Show)

data Command = PrettyPrint InputSrc
    deriving (Eq, Show)

commandParser :: Parser Command
commandParser = subparser
    (command "pretty" (info prettyPrintCommand (progDesc "pretty printer")))

prettyPrintCommand :: Parser Command
prettyPrintCommand =  PrettyPrint <$> (parseFile <|> parseStdIn)


parseFile :: Parser InputSrc
parseFile = Filename <$> argument str (
      metavar "FILE"
   <> help "filename")

parseStdIn :: Parser InputSrc
parseStdIn = flag' StdInput (short 'i' <> long "stdin")

doPrettyPrint :: InputSrc -> IO ExitCode
doPrettyPrint src = do
    fileContent <- case src of
            StdInput -> getContents
            Filename f -> readFile f

    let filename = case src of
            StdInput   -> "stdin"
            Filename f -> f

    let parsed = runParser spl filename fileContent

    case parsed of
            Left err -> do
                    putStr (parseErrorPretty err)
                    exitFailure
            Right ast -> do
                    putStrLn (pprint ast)
                    exitSuccess

main :: IO ExitCode
main = do
    opts <- execParser theOpts
    case opts of
        (PrettyPrint src) -> doPrettyPrint src
    where
        theOpts = info (commandParser <**> helper) $
               fullDesc
            <> progDesc "SPL compiler, pretty-printer and parser"
