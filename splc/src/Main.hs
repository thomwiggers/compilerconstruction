module Main (main) where

import Options.Applicative (subparser, Parser, info, execParser, fullDesc,
        helper, progDesc, (<**>), command, (<|>), argument, str, help,
        flag', short, long, metavar)
import Data.Semigroup ((<>))

import Text.Megaparsec (runParser, parseErrorPretty)
import System.Exit

import SplParser (spl)
import SplPrettyPrinter
import SplTypeChecker (runInfer, infer)
import SplIR (astToIR)
import SplSSM (showSSM)
import SplIRtoSSM (compileToSSM)

data InputSrc = StdInput | Filename String
    deriving (Eq, Show)

data Command
    = PrettyPrint InputSrc
    | Analysis InputSrc
    | SSM InputSrc
    deriving (Eq, Show)

commandParser :: Parser Command
commandParser = subparser $
    (command "pretty" (info prettyPrintCommand (progDesc "pretty printer")))
    <>
    (command "analysis" (info analysisCommand (progDesc "static analysis and type inference")))
    <>
    (command "ssm" (info compileSSMCommand (progDesc "compile to SSM")))

prettyPrintCommand :: Parser Command
prettyPrintCommand =  PrettyPrint <$> (parseFile <|> parseStdIn)

analysisCommand :: Parser Command
analysisCommand = Analysis <$> (parseFile <|> parseStdIn)

compileSSMCommand :: Parser Command
compileSSMCommand = SSM <$> (parseFile <|> parseStdIn)

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

doAnalysis :: InputSrc -> IO ExitCode
doAnalysis src = do
    fileContent <- case src of
        StdInput -> getContents
        Filename f -> readFile f
    let filename = case src of
            StdInput -> "stdin"
            Filename f -> f

    let parsed = runParser spl filename fileContent

    case parsed of
        Left err -> do
            putStr (parseErrorPretty err)
            exitFailure
        Right ast -> do
            let checked = (runInfer . infer) ast
            case checked of
                Left err -> do
                    putStrLn $ show err
                    exitFailure
                Right scheme -> do
                    putStrLn $ show scheme
                    exitSuccess

doSSM :: InputSrc -> IO ExitCode
doSSM src = do
    fileContent <- case src of
        StdInput -> getContents
        Filename f -> readFile f
    let filename = case src of
            StdInput -> "stdin"
            Filename f -> f

    let parsed = runParser spl filename fileContent

    case parsed of
        Left err -> do
            putStr (parseErrorPretty err)
            exitFailure
        Right ast -> do
            let checked = (runInfer . infer) ast
            case checked of
                Left err -> do
                    print err
                    exitFailure
                Right _ -> do
                    let compiled = compileToSSM $ astToIR ast
                    putStr $ showSSM compiled
                    exitSuccess

main :: IO ExitCode
main = do
    opts <- execParser theOpts
    case opts of
        (PrettyPrint src) -> doPrettyPrint src
        (Analysis src) -> doAnalysis src
        (SSM src) -> doSSM src
    where
        theOpts = info (commandParser <**> helper) $
               fullDesc
            <> progDesc "SPL compiler, pretty-printer and parser"
