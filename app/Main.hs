-- app/Main.hs
module Main (main) where

import System.Environment (getArgs)
import System.Exit        (exitWith, ExitCode(ExitFailure))
import System.IO          (hPutStrLn, stderr)
import Data.List          (isSuffixOf)
import Data.Maybe         (fromMaybe)

import Parser.Core        (runParser)
import Parser.XML         (parseXMLDocument)
import Parser.JSON        (parseJSONDocument)
import Format.JSON        (renderJSON)
import Format.XML         (renderXML)

usage :: String
usage = unlines
  [ "USAGE: pandoc -i <input> -f <ofmt> [-o <ofile>] [-e <ifmt>]"
  , ""
  , "  -i ifile      path to the file to convert (mandatory)"
  , "  -f ofmt       output format: xml, json, markdown (mandatory)"
  , "  -o ofile      write output to ofile (default: stdout)"
  , "  -e ifmt       input format: xml, json, markdown (default: detect by extension)"
  ]

data Options = Options
  { optIn    :: Maybe FilePath
  , optOut   :: Maybe FilePath
  , optIFmt  :: Maybe String
  , optOFmt  :: Maybe String
  }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing Nothing Nothing

parseArgs :: [String] -> Options -> IO Options
parseArgs [] opts = return opts
parseArgs ("-i":f:xs) opts = parseArgs xs opts { optIn   = Just f }
parseArgs ("-o":f:xs) opts = parseArgs xs opts { optOut  = Just f }
parseArgs ("-e":f:xs) opts = parseArgs xs opts { optIFmt = Just f }
parseArgs ("-f":f:xs) opts = parseArgs xs opts { optOFmt = Just f }
parseArgs _ _ = hPutStrLn stderr usage >> exitWith (ExitFailure 84)

detectFormat :: FilePath -> Maybe String
detectFormat fp
  | ".xml"  `isSuffixOf` fp = Just "xml"
  | ".json" `isSuffixOf` fp = Just "json"
  | ".md"   `isSuffixOf` fp = Just "markdown"
  | otherwise               = Nothing

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs args defaultOptions

  infile  <- case optIn opts of
    Just f  -> return f
    Nothing -> hPutStrLn stderr usage >> exitWith (ExitFailure 84)

  ofmt <- case optOFmt opts of
    Just f  -> return f
    Nothing -> hPutStrLn stderr usage >> exitWith (ExitFailure 84)

  let ifmt = fromMaybe
               (detectFormat infile `orFail` "cannot detect input format")
               (optIFmt opts)

  let supported = ["xml","json","markdown"]
  if ifmt `notElem` supported || ofmt `notElem` supported
    then hPutStrLn stderr ("Error: unsupported format (xml, json, markdown)") 
         >> exitWith (ExitFailure 84)
    else return ()

  input <- readFile infile

  let resultDoc = case ifmt of
        "xml"  -> runParser parseXMLDocument input
        "json" -> runParser parseJSONDocument input
        _      -> Nothing

  doc <- case resultDoc of
    Just (d,_) -> return d
    Nothing    ->
      hPutStrLn stderr ("Error: failed to parse " ++ ifmt ++ " document")
      >> exitWith (ExitFailure 84)

  let output = case ofmt of
        "json"     -> renderJSON doc
        "xml"      -> renderXML doc
        -- "markdown" -> renderMarkdown doc
        _          -> error "unreachable"

  case optOut opts of
    Just outFile -> writeFile outFile output
    Nothing      -> putStrLn output

orFail :: Maybe a -> String -> a
orFail (Just x) _  = x
orFail Nothing  msg = error msg
