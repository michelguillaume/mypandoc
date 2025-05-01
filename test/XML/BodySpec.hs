module XML.BodySpec (spec) where

import Test.Hspec
import Parser.Core       (runParser)
import Parser.XML        (parseXMLBody)
import AST                (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLBody" $ do
  it "parses body with mixed blocks and whitespace" $ do
    let input = "<body> <paragraph>P</paragraph> <codeblock>C</codeblock> </body>!"
        expected = [Paragraph [Plain "P"], CodeBlock "C"]
    runParser parseXMLBody input `shouldBe` Just (expected, "!")