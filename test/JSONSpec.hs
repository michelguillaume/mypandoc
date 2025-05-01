module JSONSpec (spec) where

import Test.Hspec
import Parser.Core           (runParser)
import Parser.JSON           (parseJSONDocument)
import AST                     (Document(..), Header(..), Block(..), Inline(..))

spec :: Spec
spec = describe "parseJSONDocument" $ do
  it "parses a minimal valid JSON document" $ do
    let input = "{\"header\":{\"title\":\"T\"}," ++ "\"body\":[\"X\"]}end"
        expected = Document (Header "T" Nothing Nothing) [Paragraph [Plain "X"]]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "end")

  it "fails when header is malformed" $ do
    let input = "{\"head\":{\"title\":\"T\"},\"body\":[\"Y\"]}"
    runParser parseJSONDocument input `shouldBe` Nothing

  it "fails when body is missing closing bracket" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[\"Y\"}"
    runParser parseJSONDocument input `shouldBe` Nothing
