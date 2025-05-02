{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module XML.CodeBlockSpec (spec) where

import Test.Hspec
import Parser.Core         (runParser)
import Parser.XML          (parseXMLCodeBlock)
import AST                  (Block(..))

spec :: Spec
spec = describe "parseXMLCodeBlock" $ do
  it "parses codeblock with one paragraph" $ do
    let input = "<codeblock><paragraph>let x=1</paragraph></codeblock>!"
    runParser parseXMLCodeBlock input
      `shouldBe` Just (CodeBlock ["let x=1"], "!")

  it "parses codeblock with multiple paragraphs" $ do
    let xml = "<codeblock>"
           ++ "<paragraph>one</paragraph>"
           ++ "<paragraph>two</paragraph>"
           ++ "</codeblock>END"
    runParser parseXMLCodeBlock xml
      `shouldBe` Just (CodeBlock ["one","two"], "END")
