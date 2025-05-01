module XML.DocumentSpec (spec) where

import Test.Hspec
import Parser.Core         (runParser)
import Parser.XML          (parseXMLDocument)
import AST                  (Document(..), Header(..), Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLDocument" $ do
  it "parses complete minimal document" $ do
    let input = concat
          [ "<document>"
          , "<header title=\"Doc\"></header>"
          , "<body><paragraph>X</paragraph></body>"
          , "</document>~" ]
        expected = Document (Header "Doc" Nothing Nothing) [Paragraph [Plain "X"]]
    runParser parseXMLDocument input `shouldBe` Just (expected, "~")
