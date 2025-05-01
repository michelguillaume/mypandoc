module XML.ParagraphSpec (spec) where

import Test.Hspec
import Parser.Core        (runParser)
import Parser.XML         (parseXMLParagraph)
import AST                 (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLParagraph" $ do
  it "parses simple plain-text paragraph" $
    runParser parseXMLParagraph "<paragraph>Hello</paragraph>!"
      `shouldBe` Just (Paragraph [Plain "Hello"], "!")

  it "parses paragraph with nested inlines" $
    runParser parseXMLParagraph
      "<paragraph>This is <bold>bold</bold> and <italic>italics</italic>.</paragraph>?"
      `shouldBe` Just
        ( Paragraph
            [ Plain "This is "
            , Bold [Plain "bold"]
            , Plain " and "
            , Italic [Plain "italics"]
            , Plain "."
            ]
        , "?"
        )
