module XML.ListItemSpec (spec) where

import Test.Hspec
import Parser.Core        (runParser)
import Parser.XML         (parseXMLListItem)
import AST                 (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLListItem" $ do
  it "parses single-block item" $ do
    let input = "<item><paragraph>One</paragraph></item>x"
    runParser parseXMLListItem input
      `shouldBe` Just ([Paragraph [Plain "One"]], "x")

  it "parses item with multiple blocks" $ do
    let body = "<paragraph>A</paragraph><codeblock>C</codeblock>"
        input = "<item>" ++ body ++ "</item>!"
    runParser parseXMLListItem input
      `shouldBe` Just ([Paragraph [Plain "A"], CodeBlock "C"], "!")
