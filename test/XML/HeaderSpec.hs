module XML.HeaderSpec (spec) where

import Test.Hspec
import Parser.Core      (runParser)
import Parser.XML       (parseXMLHeader)
import AST               (Header(..))

spec :: Spec
spec = describe "parseXMLHeader" $ do
  it "parses header with title only" $ do
    let input = "<header title=\"Test\"></header>XYZ"
    runParser parseXMLHeader input
      `shouldBe` Just (Header "Test" Nothing Nothing, "XYZ")

  it "fails when title attribute missing" $ do
    runParser parseXMLHeader "<header></header>" `shouldBe` Nothing