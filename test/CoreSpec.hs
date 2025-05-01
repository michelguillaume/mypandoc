module CoreSpec (spec) where

import Test.Hspec
import Parser.Core

-- helper to run parser on input
runP :: Parser a -> String -> Maybe (a, String)
runP = runParser

spec :: Spec
spec = do
  describe "char" $ do
    it "parses a matching char" $
      runP (char 'x') "xyz" `shouldBe` Just ('x', "yz")

    it "fails on non-matching char" $
      runP (char 'x') "abc" `shouldBe` Nothing

  describe "string" $ do
    it "parses a full string" $
      runP (string "hello") "hello world" `shouldBe` Just ("hello", " world")

    it "fails if prefix doesn't match" $
      runP (string "hi") "hello" `shouldBe` Nothing

  describe "parseQuoted" $ do
    it "parses a quoted string" $
      runP parseQuoted "\"abc123\"tail" `shouldBe` Just ("abc123", "tail")
