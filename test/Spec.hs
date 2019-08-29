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
      parseSucc "( 1 + 2 ) * 3;" `shouldBe` Stmt [Mul (Add (Nat 1) (Nat 2)) (Nat 3)]
      parseSucc "2 + -3;" `shouldBe` Stmt [Add (Nat 2) (Sub (Nat 0) (Nat 3))]
      parseSucc "2 * +3;" `shouldBe` Stmt [Mul (Nat 2) (Nat 3)]

    it "Add, Sub, Mul, Div" $ do
      parseSucc "1 + 2 - 3 ;" `shouldBe` Stmt [Sub (Add (Nat 1) (Nat 2)) (Nat 3)]
      parseSucc "1 * 2 / 3 ;" `shouldBe` Stmt [Div (Mul (Nat 1) (Nat 2)) (Nat 3)]

    it "relational" $ do
      parseSucc "1 < 2;" `shouldBe` Stmt [Lt (Nat 1) (Nat 2)]
      parseSucc "1 <= 2;" `shouldBe` Stmt [Lte (Nat 1) (Nat 2)]
      parseSucc "1 > 2;" `shouldBe` Stmt [Gt (Nat 1) (Nat 2)]
      parseSucc "1 >= 2;" `shouldBe` Stmt [Gte (Nat 1) (Nat 2)]
      parseSucc "1*2 >= 2+3;" `shouldBe` Stmt [Gte (Mul (Nat 1) (Nat 2)) (Add (Nat 2) (Nat 3))]

    it "Assign" $ do
      parseSucc "x = 2*3-2/3;" `shouldBe` Stmt [Assign "x" (Sub (Mul (Nat 2) (Nat 3)) (Div (Nat 2) (Nat 3)))]
      parseSucc "x;" `shouldBe` Stmt [LVar "x"]