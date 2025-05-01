module JSON.StringSpec (spec) where

import Test.Hspec
import Parser.Core            (runParser)
import Parser.JSON            (parseJSONStringBlock)
import AST                      (Block(..), Inline(..))

spec :: Spec
spec = describe "parseJSONStringBlock" $ do
  it "parses a quoted string into a paragraph block" $ do
    let input = "\"foo\"bar"
    runParser parseJSONStringBlock input
      `shouldBe` Just (Paragraph [Plain "foo"], "bar")

  it "parses empty quoted string" $ do
    let input = "\"\"rest"
    runParser parseJSONStringBlock input
      `shouldBe` Just (Paragraph [Plain ""], "rest")