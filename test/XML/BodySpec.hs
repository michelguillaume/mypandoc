module XML.BodySpec (spec) where

import Test.Hspec
import Parser.Core      (runParser)
import Parser.XML       (parseXMLBody)
import AST               (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLBody" $ do
  it "parses body with mixed blocks" $ do
    let xml = "<body> "
           ++ "<paragraph>P</paragraph> "
           ++ "<codeblock><paragraph>C</paragraph></codeblock> "
           ++ "</body>!"
    runParser parseXMLBody xml
      `shouldBe` Just ([Paragraph [Plain "P"], CodeBlock ["C"]], "!")
