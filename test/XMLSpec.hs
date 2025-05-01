-- test/XMLSpec.hs
module XMLSpec (spec) where

import Test.Hspec
import Parser.Core (runParser)
import Parser.XML
import AST (Header(..), Block(..), Inline(..), Document(..), ListType(..))

spec :: Spec
spec =
  describe "XML parsers" $ do

    describe "parseXMLHeader" $ do
      it "parses header with title only" $ do
        let input = "<header title=\"Test\"></header>XYZ"
        runParser parseXMLHeader input
          `shouldBe` Just (Header "Test" Nothing Nothing, "XYZ")

      it "fails if title attribute missing" $ do
        runParser parseXMLHeader "<header></header>" `shouldBe` Nothing

    describe "parseXMLParagraph" $ do
      it "parses simple paragraph" $ do
        let input = "<paragraph>Hello</paragraph>!"
        runParser parseXMLParagraph input
          `shouldBe` Just (Paragraph [Plain "Hello"], "!")

      it "parses empty paragraph" $ do
        let input = "<paragraph></paragraph>tail"
        runParser parseXMLParagraph input
          `shouldBe` Just (Paragraph [Plain ""], "tail")

    describe "parseXMLCodeBlock" $ do
      it "parses code block content" $ do
        let input = "<codeblock>let x = 1</codeblock>!"
        runParser parseXMLCodeBlock input
          `shouldBe` Just (CodeBlock "let x = 1", "!")

      it "parses empty code block" $ do
        let input = "<codeblock></codeblock>rest"
        runParser parseXMLCodeBlock input
          `shouldBe` Just (CodeBlock "", "rest")

    describe "parseXMLListItem" $ do
      it "parses single-block item" $ do
        let input = "<item><paragraph>One</paragraph></item>x"
        runParser parseXMLListItem input
          `shouldBe` Just ([Paragraph [Plain "One"]], "x")

      it "parses item with multiple blocks" $ do
        let body = "<paragraph>A</paragraph><codeblock>C</codeblock>"
            input = "<item>" ++ body ++ "</item>!"
        runParser parseXMLListItem input
          `shouldBe` Just ([Paragraph [Plain "A"], CodeBlock "C"], "!")

    describe "parseXMLList" $ do
      it "parses ordered list with two items" $ do
        let items = "<item><paragraph>1</paragraph></item><item><paragraph>2</paragraph></item>"
            input = "<list type=\"ordered\">" ++ items ++ "</list>end"
            expected = List Ordered [[Paragraph [Plain "1"]], [Paragraph [Plain "2"]]]
        runParser parseXMLList input `shouldBe` Just (expected, "end")

      it "parses unordered empty list" $ do
        let input = "<list type=\"unordered\"></list>!"
        runParser parseXMLList input
          `shouldBe` Just (List Unordered [], "!")

    describe "parseXMLSection" $ do
      it "parses section with title and blocks" $ do
        let body = "<paragraph>Sec</paragraph>"
            input = "<section><title>MySec</title>" ++ body ++ "</section>#"
            expected = Section (Just "MySec") [Paragraph [Plain "Sec"]]
        runParser parseXMLSection input `shouldBe` Just (expected, "#")

      it "parses section without title" $ do
        let input = "<section><paragraph>NoTitle</paragraph></section>?"
            expected = Section Nothing [Paragraph [Plain "NoTitle"]]
        runParser parseXMLSection input `shouldBe` Just (expected, "?")

    describe "parseXMLBody" $ do
      it "parses body with multiple blocks" $ do
        let blocks = "<paragraph>P</paragraph><codeblock>C</codeblock>"
            input = "<body>" ++ blocks ++ "</body>!"
            expected = [Paragraph [Plain "P"], CodeBlock "C"]
        runParser parseXMLBody input `shouldBe` Just (expected, "!")

    describe "parseXMLDocument" $ do
      it "parses full minimal document" $ do
        let input = concat
              [ "<document>"
              , "<header title=\"Doc\"></header>"
              , "<body><paragraph>X</paragraph></body>"
              , "</document>~"
              ]
            expectedDoc = Document (Header "Doc" Nothing Nothing)
                                  [Paragraph [Plain "X"]]
        runParser parseXMLDocument input `shouldBe` Just (expectedDoc, "~")
