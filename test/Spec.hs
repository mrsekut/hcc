import           Text.Parsec
import           Test.Hspec
import           Parser


parseP :: String -> Program
parseP input = case parse program "Example" input of
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
    parseExpr "( 1 + 2 ) * 3;" `shouldBe` Mul (Add (Nat 1) (Nat 2)) (Nat 3)
    parseExpr "2 + -3;"        `shouldBe` Add (Nat 2) (Sub (Nat 0) (Nat 3))
    parseExpr "2 * +3;"        `shouldBe` Mul (Nat 2) (Nat 3)

  it "Add, Sub, Mul, Div" $ do
    parseExpr "1 + 2 - 3 ;" `shouldBe` Sub (Add (Nat 1) (Nat 2)) (Nat 3)
    parseExpr "1 * 2 / 3 ;" `shouldBe` Div (Mul (Nat 1) (Nat 2)) (Nat 3)

  it "relational" $ do
    parseExpr "1 < 2;"  `shouldBe` Lt  (Nat 1) (Nat 2)
    parseExpr "1 <= 2;" `shouldBe` Lte (Nat 1) (Nat 2)
    parseExpr "1 > 2;"  `shouldBe` Gt  (Nat 1) (Nat 2)
    parseExpr "1 >= 2;" `shouldBe` Gte (Nat 1) (Nat 2)
    parseExpr "1*2 >= 2+3;"
      `shouldBe` Gte (Mul (Nat 1) (Nat 2)) (Add (Nat 2) (Nat 3))

  it "Assign" $ do
    parseStmt "x = 2*3-2/3;"
      `shouldBe` Assign "x" (Sub (Mul (Nat 2) (Nat 3)) (Div (Nat 2) (Nat 3)))
    parseStmt "x;" `shouldBe` S [LVar "x"]
    parseP "x = 3; x;" `shouldBe` Program [Assign "x" (Nat 3), S [LVar "x"]]
