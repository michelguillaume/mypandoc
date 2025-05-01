module XML.CodeBlockSpec (spec) where

import Test.Hspec
import Parser.Core         (runParser)
import Parser.XML          (parseXMLCodeBlock)
import AST                  (Block(..))

spec :: Spec
spec = describe "parseXMLCodeBlock" $ do
  it "parses code block with content" $ do
    let input = "<codeblock>let x = 1</codeblock>!"
    runParser parseXMLCodeBlock input
      `shouldBe` Just (CodeBlock "let x = 1", "!")

  it "parses empty code block" $ do
    let input = "<codeblock></codeblock>rest"
    runParser parseXMLCodeBlock input
      `shouldBe` Just (CodeBlock "", "rest")