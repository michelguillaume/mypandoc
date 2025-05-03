{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(ExitFailure))
import System.IO          (hPutStrLn, stderr)
import Data.List          (isSuffixOf)
import Data.Maybe         (fromMaybe)
import Control.Monad      (when)

import Parser.Core        (runParser)
import AST                (Document)
import Parser.XML         (parseXMLDocument)
import Parser.JSON        (parseJSONDocument)
import Parser.Markdown    (parseMarkdownDocument)
import Format.JSON        (renderJSON)
import Format.XML         (renderXML)
import Format.Markdown    (renderMarkdown)

-- Usage header (<=10 lines)
usageHeader :: [String]
usageHeader =
  [ "USAGE: pandoc -i <input> -f <ofmt> [-o <ofile>]"
  , "       [-e <ifmt>]"
  , ""
  ]

-- Usage details (<=10 lines)
usageDetail :: [String]
usageDetail =
  [ "  -i ifile   path to file to convert (mandatory)"
  , "  -f ofmt    output format: xml, json, markdown"
  , "             (mandatory)"
  , "  -o ofile   write output to file (default: stdout)"
  , "  -e ifmt    input format: xml, json, markdown"
  , "             (default: detect by extension)"
  ]

-- Combined usage lines
usageLines :: [String]
usageLines = usageHeader ++ usageDetail

-- Usage message
usage :: String
usage = unlines usageLines

-- Options record
data Options = Options
  { optIn   :: Maybe FilePath
  , optOut  :: Maybe FilePath
  , optIFmt :: Maybe String
  , optOFmt :: Maybe String
  }

-- Default CLI options
defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing Nothing

-- Parse command-line arguments
getOptions :: IO Options
getOptions = do
  args <- getArgs
  parseArgs args defaultOptions

-- ParseArgs helper
type Args = [String]
parseArgs :: Args -> Options -> IO Options
parseArgs [] opts = return opts
parseArgs ("-i":f:xs) opts = parseArgs xs opts { optIn   = Just f }
parseArgs ("-o":f:xs) opts = parseArgs xs opts { optOut  = Just f }
parseArgs ("-e":f:xs) opts = parseArgs xs opts { optIFmt = Just f }
parseArgs ("-f":f:xs) opts = parseArgs xs opts { optOFmt = Just f }
parseArgs _ _ = dieUsage

-- Detect format from file extension
detectFormat :: FilePath -> Maybe String
detectFormat fp
  | ".xml"  `isSuffixOf` fp = Just "xml"
  | ".json" `isSuffixOf` fp = Just "json"
  | ".md"   `isSuffixOf` fp = Just "markdown"
  | otherwise               = Nothing

-- Error helper for missing values
orFail :: Maybe a -> String -> a
orFail (Just x) _   = x
orFail Nothing  msg = error msg

-- Print usage and exit
dieUsage :: IO a
dieUsage = hPutStrLn stderr usage >>
           exitWith (ExitFailure 84)

-- Main entry (<=10 lines)
main :: IO ()
main = do
  opts    <- getOptions
  infile  <- getInput opts
  ofmt    <- getOutput opts
  let ifmt = fromMaybe
               (detectFormat infile `orFail`
                "cannot detect input format")
               (optIFmt opts)
  validateFormats ifmt ofmt
  process infile ifmt ofmt opts

-- Retrieve input file or exit
getInput :: Options -> IO FilePath
getInput opts =
  case optIn opts of
    Just f  -> return f
    Nothing -> dieUsage

-- Retrieve output format or exit
getOutput :: Options -> IO String
getOutput opts =
  case optOFmt opts of
    Just f  -> return f
    Nothing -> dieUsage

-- Check supported formats
validateFormats :: String -> String -> IO ()
validateFormats ifmt ofmt =
  let sup = ["xml","json","markdown"] in
  when (ifmt `notElem` sup || ofmt `notElem` sup) dieUsage

-- Orchestrate parsing and rendering
process :: FilePath -> String -> String -> Options -> IO ()
process infile ifmt ofmt opts = do
  content <- readFile infile
  doc     <- parseDoc ifmt content
  let output = renderDoc ofmt doc
  writeOut opts output

-- Parse according to input format
parseDoc :: String -> String -> IO Document
parseDoc fmt input =
  let res = case fmt of
        "xml"      -> runParser parseXMLDocument input
        "json"     -> runParser parseJSONDocument input
        "markdown" -> runParser parseMarkdownDocument input
        _           -> Nothing
  in case res of
       Just (d,_) -> return d
       Nothing    -> dieParse fmt

-- Error on parse failure
dieParse :: String -> IO a
dieParse fmt =
  hPutStrLn stderr ("Error: failed to parse " ++ fmt ++ " document") >>
  exitWith (ExitFailure 84)

-- Render based on output format
renderDoc :: String -> Document -> String
renderDoc fmt doc =
  case fmt of
    "json" -> renderJSON doc
    "xml"  -> renderXML doc
    "markdown" -> renderMarkdown doc
    _       -> error "unreachable"

-- Write to file or stdout
writeOut :: Options -> String -> IO ()
writeOut opts out =
  case optOut opts of
    Just f  -> writeFile f out
    Nothing -> putStrLn out
