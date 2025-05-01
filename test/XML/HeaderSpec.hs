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
      `shouldBe` Just (Header "Test" Nothing     Nothing, "XYZ")

  it "parses header with author and date" $ do
    let input = "<header title=\"Doc\">" ++
                "<author>Leo</author>" ++
                "<date>2021-12-31</date>" ++
                "</header>!"
    runParser parseXMLHeader input
      `shouldBe` Just (Header "Doc" (Just "Leo") (Just "2021-12-31"), "!")
