{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module JSONSpec (spec) where

import Test.Hspec
import Parser.Core    (runParser)
import Parser.JSON    (parseJSONDocument)
import AST            ( Document(..)
                     , Header(..)
                     , Block(..)
                     , Inline(..)
                     )

spec :: Spec
spec = describe "parseJSONDocument" $ do

  it "parses a minimal valid JSON document" $ do
    let input    = "{\"header\":{\"title\":\"T\"},\"body\":[[\"X\"]]}end"
        expected = Document (Header "T" Nothing Nothing)
                             [ Paragraph [Plain "X"] ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "end")

  it "fails when header is malformed" $ do
    let input = "{\"head\":{\"title\":\"T\"},\"body\":[[\"Y\"]]}"
    runParser parseJSONDocument input
      `shouldBe` Nothing

  it "fails when body is missing closing bracket" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[[\"Y\"]}"
    runParser parseJSONDocument input
      `shouldBe` Nothing

  it "parses header with author and date" $ do
    let input    = "{\"header\":{\"title\":\"T\",\"author\":\"A\",\"date\":\"D\"},\"body\":[]}!"
        expected = Document (Header "T" (Just "A") (Just "D")) []
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "!")

  it "parses multiple paragraphs" $ do
    let input    = "{\"header\":{\"title\":\"T\"},\"body\":[[\"A\"],[\"B\"]]}XYZ"
        expected = Document (Header "T" Nothing Nothing)
                             [ Paragraph [Plain "A"]
                             , Paragraph [Plain "B"]
                             ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "XYZ")

  it "parses a nested section" $ do
    let input =
          "{\"header\":{\"title\":\"T\"},\"body\":[\
          \{\"section\":{\"title\":\"S\",\"content\":[[\"X\"]]}}]}#"
        expected = Document (Header "T" Nothing Nothing)
                            [ Section "S" [Paragraph [Plain "X"]] ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "#")

  it "parses a codeblock" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[\
                \ {\"codeblock\":[\"c1\",\"c2\"]}]}?"
        expected = Document (Header "T" Nothing Nothing)
                             [ CodeBlock ["c1","c2"] ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "?")

  it "parses a list" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[\
                \ {\"list\":[[\"i1\"],[\"i2\"]]}]}@"
        expected = Document (Header "T" Nothing Nothing)
                             [ List [[Plain "i1"], [Plain "i2"]] ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "@")

  it "parses mixed inline elements in a paragraph" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[["
             ++ "\" before \","
             ++ "{\"bold\":\"B\"},"
             ++ "\", \","
             ++ "{\"italic\":\"I\"},"
             ++ "\" and \","
             ++ "{\"code\":\"C\"},"
             ++ "\" after \""
             ++ "]]}$"
        expected = Document (Header "T" Nothing Nothing)
          [ Paragraph
              [ Plain " before "
              , Bold   [Plain "B"]
              , Plain ", "
              , Italic [Plain "I"]
              , Plain " and "
              , CodeSpan "C"
              , Plain " after "
              ]
          ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "$")

  it "parses a link and an image inline" $ do
    let input = "{\"header\":{\"title\":\"T\"},\"body\":[["
             ++ "\"Go to \","
             ++ "{\"link\":{\"url\":\"u\",\"content\":[\"L\"]}},"
             ++ "\", see \","
             ++ "{\"image\":{\"url\":\"img\",\"alt\":[\"alt\"]}},"
             ++ "\"!\""
             ++ "]]}~"
        expected = Document (Header "T" Nothing Nothing)
          [ Paragraph
              [ Plain "Go to "
              , Link  "u" [Plain "L"]
              , Plain ", see "
              , Image "img" [Plain "alt"]
              , Plain "!" 
              ]
          ]
    runParser parseJSONDocument input
      `shouldBe` Just (expected, "~")
