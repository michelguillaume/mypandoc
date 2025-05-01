module JSON.ArraySpec (spec) where

import Test.Hspec
import Parser.Core            (runParser)
import Parser.JSON            (parseJSONArrayBody)
import AST                      (Block(..), Inline(..))

spec :: Spec
spec = describe "parseJSONArrayBody" $ do
  it "parses an empty array" $ do
    let input = "[]XYZ"
    runParser parseJSONArrayBody input
      `shouldBe` Just ([], "XYZ")

  it "parses a single-element array" $ do
    let input = "[\"one\"]more"
    runParser parseJSONArrayBody input
      `shouldBe` Just ([Paragraph [Plain "one"]], "more")

  it "fails on multi-element array (comma not supported)" $ do
    let input = "[\"a\",\"b\"]end"
    runParser parseJSONArrayBody input
      `shouldBe` Nothing