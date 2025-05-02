{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module XML.InlineSpec (spec) where

import Test.Hspec
import Parser.Core      (runParser)
import Parser.XML       (parseInline)
import AST               (Inline(..))

spec :: Spec
spec = describe "parseInline" $ do

  it "parses plain text" $
    runParser parseInline "abc<" `shouldBe` Just (Plain "abc", "<")

  it "parses bold" $
    runParser parseInline "<bold>STR</bold>x"
      `shouldBe` Just (Bold [Plain "STR"], "x")

  it "parses italic" $
    runParser parseInline "<italic>IT</italic>!"
      `shouldBe` Just (Italic [Plain "IT"], "!")

  it "parses inline code" $
    runParser parseInline "<code>c()</code>?"
      `shouldBe` Just (CodeSpan "c()", "?")

  it "parses link" $
    runParser parseInline
      "<link url=\"u\"><bold>bt</bold></link>tail"
      `shouldBe` Just
        ( Link "u" [Bold [Plain "bt"]]
        , "tail"
        )

  it "parses image" $
    runParser parseInline
      "<image url=\"img.png\">alt text</image>END"
      `shouldBe` Just
        ( Image "img.png" [Plain "alt text"]
        , "END"
        )
