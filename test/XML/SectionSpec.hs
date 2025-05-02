{-
-- EPITECH PROJECT, 2025
-- Haskell
-- File description:
-- main
-}

module XML.SectionSpec (spec) where

import Test.Hspec
import Parser.Core       (runParser)
import Parser.XML        (parseXMLSection)
import AST                (Block(..), Inline(..))

spec :: Spec
spec = describe "parseXMLSection" $ do
  it "parses section with title attribute" $
    runParser parseXMLSection
      "<section title=\"S1\"><paragraph>X</paragraph></section>#"
      `shouldBe` Just (Section "S1" [Paragraph [Plain "X"]], "#")

  it "parses section without title attribute" $
    runParser parseXMLSection
      "<section><paragraph>Y</paragraph></section>?"
      `shouldBe` Just (Section ""  [Paragraph [Plain "Y"]], "?")
