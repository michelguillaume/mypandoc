{-# LANGUAGE OverloadedStrings #-}
module IntegrationSpec (spec) where

import Test.Hspec
import Control.Monad        (forM_)
import System.Process       (readProcessWithExitCode)
import System.Exit          (ExitCode(ExitSuccess))
import System.Directory     (listDirectory)
import System.FilePath      (takeBaseName, replaceExtension, (</>), takeExtension)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

spec :: Spec
spec = describe "CLI end-to-end" $ do
  -- Découvre tous les .xml dans examples/
  xmlFiles <- runIO $ listDirectory "examples"
  let xmls = filter ((== ".xml") . takeExtension) xmlFiles

  forM_ xmls $ \xmlName -> do
    let base     = takeBaseName xmlName
        xmlPath  = "examples" </> xmlName
        jsonPath = "examples" </> replaceExtension xmlName "json"

    it ("XML → JSON for " ++ base) $ do
      xmlIn   <- TIO.readFile xmlPath
      jsonExp <- TIO.readFile jsonPath

      (code, out, err) <- readProcessWithExitCode
                            "./mypandoc"
                            ["-i", xmlPath, "-e", "xml", "-f", "json"]
                            ""
      code `shouldBe` ExitSuccess
      err  `shouldBe` ""
      T.strip (T.pack out) `shouldBe` T.strip jsonExp

    it ("JSON → XML for " ++ base) $ do
      jsonIn <- TIO.readFile jsonPath
      xmlExp  <- TIO.readFile xmlPath

      (code, out, err) <- readProcessWithExitCode
                            "./mypandoc"
                            ["-i", jsonPath, "-e", "json", "-f", "xml"]
                            ""
      code `shouldBe` ExitSuccess
      err  `shouldBe` ""
      T.strip (T.pack out) `shouldBe` T.strip xmlExp
