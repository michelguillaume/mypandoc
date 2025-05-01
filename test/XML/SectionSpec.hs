module XML.SectionSpec (spec) where

import Test.Hspec
import Parser.Core        (runParser)
import Parser.XML         (parseXMLSection)
import AST                 (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLSection" $ do
  it "parses section with title and one block" $ do
    let body = "<paragraph>Sec</paragraph>"
        input = "<section><title>MySec</title>" ++ body ++ "</section>#"
        expected = Section (Just "MySec") [Paragraph [Plain "Sec"]]
    runParser parseXMLSection input `shouldBe` Just (expected, "#")

  it "parses section without title" $ do
    let input = "<section><paragraph>NoTitle</paragraph></section>?"
        expected = Section Nothing [Paragraph [Plain "NoTitle"]]
    runParser parseXMLSection input `shouldBe` Just (expected, "?")
