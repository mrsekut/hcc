module LexerSpec (spec) where

import Test.Hspec
import Lexer

spec :: Spec
spec = do
  describe "Lexer" $ do
    it "number and `+` and `-`" $ do
      lexer "3" `shouldBe` ([Token {tokenType = TK_NUM, valueString = "3"}] :: [Token])
