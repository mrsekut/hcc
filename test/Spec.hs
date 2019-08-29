import           Text.Parsec
import           Test.Hspec
import           Parser


parseSucc :: String -> Program
parseSucc input = case parse program "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


parseStmt :: String -> Stmt
parseStmt input = case parse stmt "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


parseExpr :: String -> Expr
parseExpr input = case parse expr "Example" input of
  Left{}    -> error "Parse failure"
  Right str -> str


-- parseFail :: String -> String
-- parseFail input = case parse program "Example" input of
--   Left{}  -> "no parse"
--   Right _ -> error "Parsed but shouldn't"


main :: IO ()
main = hspec $ describe "Parser" $ do
  it "factor" $ do
    parseExpr "( 1 + 2 ) * 3;" `shouldBe` B Mul (B Add (Nat 1) (Nat 2)) (Nat 3)
    parseExpr "2 + -3;" `shouldBe` B Add (Nat 2) (B Sub (Nat 0) (Nat 3))
    parseExpr "2 * +3;" `shouldBe` B Mul (Nat 2) (Nat 3)

  it "Add, Sub, Mul, Div" $ do
    parseExpr "1 + 2 - 3 ;" `shouldBe` B Sub (B Add (Nat 1) (Nat 2)) (Nat 3)
    parseExpr "1 * 2 / 3 ;" `shouldBe` B Div (B Mul (Nat 1) (Nat 2)) (Nat 3)

  it "relational" $ do
    parseExpr "1 < 2;" `shouldBe` B Lt (Nat 1) (Nat 2)
    parseExpr "1 <= 2;" `shouldBe` B Lte (Nat 1) (Nat 2)
    parseExpr "1 > 2;" `shouldBe` B Gt (Nat 1) (Nat 2)
    parseExpr "1 >= 2;" `shouldBe` B Gte (Nat 1) (Nat 2)
    parseExpr "1*2 >= 2+3;"
      `shouldBe` B Gte (B Mul (Nat 1) (Nat 2)) (B Add (Nat 2) (Nat 3))

  it "Assign" $ do
    parseStmt "x = 2*3-2/3;" `shouldBe` Assign "x" (B Sub (B Mul (Nat 2) (Nat 3)) (B Div (Nat 2) (Nat 3)))
    parseStmt "x;" `shouldBe` S [LVar "x"]
    -- parseStmt "x = 3; x;" `shouldBe` S [LVar "x"] -- TODO:
