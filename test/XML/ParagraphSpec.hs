module XML.ParagraphSpec (spec) where

import Test.Hspec
import Parser.Core         (runParser)
import Parser.XML          (parseXMLParagraph)
import AST                  (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLParagraph" $ do
  it "parses simple paragraph" $ do
    let input = "<paragraph>Hello</paragraph>!"
    runParser parseXMLParagraph input
      `shouldBe` Just (Paragraph [Plain "Hello"], "!")

  it "parses empty paragraph" $ do
    let input = "<paragraph></paragraph>tail"
    runParser parseXMLParagraph input
      `shouldBe` Just (Paragraph [Plain ""], "tail")