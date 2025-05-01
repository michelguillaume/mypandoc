module XMLSpec (spec) where

import Test.Hspec
import Parser.Core        (runParser)
import Parser.XML         ( parseXMLHeader
                          , parseXMLParagraph
                          , parseXMLCodeBlock
                          , parseXMLList
                          , parseXMLSection
                          , parseXMLBody
                          , parseXMLDocument
                          )
import AST                ( Header(..)
                          , Block(..)
                          , Inline(..)
                          , Document(..)
                          )

spec :: Spec
spec = describe "XML parsers" $ do

  describe "parseXMLHeader" $ do
    it "parses header with title only" $ do
      let input = "<header title=\"Test\"></header>XYZ"
      runParser parseXMLHeader input
        `shouldBe` Just (Header "Test" Nothing Nothing, "XYZ")

    it "parses header with author and date" $ do
      let input = "<header title=\"D\">"
               ++ "<author>A</author>"
               ++ "<date>2021-12-31</date>"
               ++ "</header>!"
      runParser parseXMLHeader input
        `shouldBe` Just (Header "D" (Just "A") (Just "2021-12-31"), "!")

  describe "parseXMLParagraph" $ do
    it "parses simple paragraph" $ do
      let input = "<paragraph>Hello</paragraph>!"
      runParser parseXMLParagraph input
        `shouldBe` Just (Paragraph [Plain "Hello"], "!")

    it "parses paragraph with nested inlines" $ do
      let input = "<paragraph>This is <bold>B</bold> and <italic>I</italic>.</paragraph>X"
      runParser parseXMLParagraph input
        `shouldBe` Just
          ( Paragraph
              [ Plain  "This is "
              , Bold  [Plain "B"]
              , Plain  " and "
              , Italic [Plain "I"]
              , Plain  "."
              ]
          , "X"
          )

  describe "parseXMLCodeBlock" $ do
    it "parses codeblock with one paragraph" $ do
      let input = "<codeblock><paragraph>one</paragraph></codeblock>!"
      runParser parseXMLCodeBlock input
        `shouldBe` Just (CodeBlock ["one"], "!")

    it "parses codeblock with multiple paragraphs" $ do
      let xml = "<codeblock>"
             ++ "<paragraph>a</paragraph>"
             ++ "<paragraph>b</paragraph>"
             ++ "</codeblock>END"
      runParser parseXMLCodeBlock xml
        `shouldBe` Just (CodeBlock ["a","b"], "END")

  describe "parseXMLList" $ do
    it "parses empty list" $
      runParser parseXMLList "<list></list>!"
        `shouldBe` Just (List [], "!")

    it "parses list of paragraphs" $ do
      let xml = "<list>"
             ++ "<paragraph>i1</paragraph>"
             ++ "<paragraph>i2</paragraph>"
             ++ "</list>K"
      runParser parseXMLList xml
        `shouldBe` Just (List [[Plain "i1"], [Plain "i2"]], "K")

  describe "parseXMLSection" $ do
    it "parses section with title attribute" $ do
      let xml = "<section title=\"S1\"><paragraph>X</paragraph></section>#"
      runParser parseXMLSection xml
        `shouldBe` Just (Section "S1" [Paragraph [Plain "X"]], "#")

    it "parses section without title" $ do
      let xml = "<section><paragraph>Y</paragraph></section>?"
      runParser parseXMLSection xml
        `shouldBe` Just (Section "" [Paragraph [Plain "Y"]], "?")

  describe "parseXMLBody" $ do
    it "parses body with mixed blocks and whitespace" $ do
      let xml = "<body> "
             ++ "<paragraph>P</paragraph> "
             ++ "<codeblock><paragraph>C</paragraph></codeblock> "
             ++ "</body>!"
      runParser parseXMLBody xml
        `shouldBe` Just ([Paragraph [Plain "P"], CodeBlock ["C"]], "!")

  describe "parseXMLDocument" $ do
    it "parses minimal document" $ do
      let xml = concat
            [ "<document>"
            , "<header title=\"Doc\"></header>"
            , "<body><paragraph>A</paragraph></body>"
            , "</document>."
            ]
          expected = Document (Header "Doc" Nothing Nothing)
                              [Paragraph [Plain "A"]]
      runParser parseXMLDocument xml
        `shouldBe` Just (expected, ".")
