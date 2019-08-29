import           Text.Parsec
import           Test.Hspec
import           Parser


parseSucc :: String -> Stmt
parseSucc input = case parse program "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


-- parseFail :: String -> String
-- parseFail input = case parse program "Example" input of
--   Left{}  -> "no parse"
--   Right _ -> error "Parsed but shouldn't"


main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "factor" $ do
      parseSucc "( 1 + 2 ) * 3;"
        `shouldBe` Stmt [B Mul (B Add (Nat 1) (Nat 2)) (Nat 3)]
      parseSucc "2 + -3;"
        `shouldBe` Stmt [B Add (Nat 2) (B Sub (Nat 0) (Nat 3))]
      parseSucc "2 * +3;" `shouldBe` Stmt [B Mul (Nat 2) (Nat 3)]

    it "Add, Sub, Mul, Div" $ do
      parseSucc "1 + 2 - 3 ;"
        `shouldBe` Stmt [B Sub (B Add (Nat 1) (Nat 2)) (Nat 3)]
      parseSucc "1 * 2 / 3 ;"
        `shouldBe` Stmt [B Div (B Mul (Nat 1) (Nat 2)) (Nat 3)]

    it "relational" $ do
      parseSucc "1 < 2;" `shouldBe` Stmt [B Lt (Nat 1) (Nat 2)]
      parseSucc "1 <= 2;" `shouldBe` Stmt [B Lte (Nat 1) (Nat 2)]
      parseSucc "1 > 2;" `shouldBe` Stmt [B Gt (Nat 1) (Nat 2)]
      parseSucc "1 >= 2;" `shouldBe` Stmt [B Gte (Nat 1) (Nat 2)]
      parseSucc "1*2 >= 2+3;" `shouldBe` Stmt
        [B Gte (B Mul (Nat 1) (Nat 2)) (B Add (Nat 2) (Nat 3))]

    it "Assign" $ do
      parseSucc "x = 2*3-2/3;" `shouldBe` Stmt
        [Assign "x" (B Sub (B Mul (Nat 2) (Nat 3)) (B Div (Nat 2) (Nat 3)))]
      parseSucc "x;" `shouldBe` Stmt [LVar "x"]
