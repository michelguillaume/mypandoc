module XML.DocumentSpec (spec) where

import Test.Hspec
import Parser.Core        (runParser)
import Parser.XML         (parseXMLDocument)
import AST                 (Document(..), Header(..), Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLDocument" $ do
  it "parses a minimal document" $ do
    let xml = concat
          [ "<document>"
          , "<header title=\"D\"></header>"
          , "<body><paragraph>A</paragraph></body>"
          , "</document>!"
          ]
        expected = Document (Header "D" Nothing Nothing) [Paragraph [Plain "A"]]
    runParser parseXMLDocument xml
      `shouldBe` Just (expected, "!")
