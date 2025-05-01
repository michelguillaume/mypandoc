module XML.ListSpec (spec) where

import Test.Hspec
import Parser.Core       (runParser)
import Parser.XML        (parseXMLList)
import AST                (Block(..), ListType(..), Inline(..))

spec :: Spec
spec = describe "parseXMLList" $ do
  it "parses ordered list with two items" $ do
    let items = "<item><paragraph>1</paragraph></item><item><paragraph>2</paragraph></item>"
        input = "<list type=\"ordered\">" ++ items ++ "</list>end"
        expected = List Ordered [[Paragraph [Plain "1"]], [Paragraph [Plain "2"]]]
    runParser parseXMLList input `shouldBe` Just (expected, "end")

  it "parses unordered empty list" $ do
    let input = "<list type=\"unordered\"></list>!"
    runParser parseXMLList input
      `shouldBe` Just (List Unordered [], "!")

  it "parses nested lists" $ do
    let inner = "<list type=\"unordered\"><item><paragraph>i1</paragraph></item></list>"
        outer = "<list type=\"ordered\"><item>" ++ inner ++ "</item></list>?"
        expected = List Ordered [[List Unordered [[Paragraph [Plain "i1"]]]]]
    runParser parseXMLList outer `shouldBe` Just (expected, "?")