module ParserSpec
  ( spec
  )
where

import Text.Parsec
import           Test.Hspec
import           Parser

-- spec :: Spec
-- spec = do
--   describe "Parser" $ do
--     it "parse" $ do
--       expr "3+3" `shouldBe` (Add (Nat 3) (Nat 3))