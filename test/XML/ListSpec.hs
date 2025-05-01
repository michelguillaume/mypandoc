module XML.ListSpec (spec) where

import Test.Hspec
import Parser.Core      (runParser)
import Parser.XML       (parseXMLList)
import AST               (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLList" $ do
  it "parses empty list" $
    runParser parseXMLList "<list></list>!"
      `shouldBe` Just (List [], "!")

  it "parses list of paragraphs" $ do
    let xml = "<list>"
           ++ "<paragraph>i1</paragraph>"
           ++ "<paragraph>i2</paragraph>"
           ++ "</list>R"
    runParser parseXMLList xml
      `shouldBe` Just (List [[Plain "i1"], [Plain "i2"]], "R")
