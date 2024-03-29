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
    -- parseP "x x x;" `shouldBe`  -- NG

  it "return" $ do
    parseStmt "return  3*3;"  `shouldBe` Return (Mul (Nat 3) (Nat 3))
    -- parseExpr "return3;"  `shouldBe` -- OK: `return3`という変数名 PENDING: 現状は変数名に数値は入れられないので保留
    -- parseExpr "return3*3;"  `shouldBe` -- OK: `hoge * 3`と同じ形

  it "if" $ do
    parseStmt "if ( 2>1 ) { return 4; } else { return 5; };"
      `shouldBe` If (Gt (Nat 2) (Nat 1)) (Return (Nat 4)) (Return (Nat 5))
    parseStmt "if ( 2>1 ) { return 4; };"
      `shouldBe` If (Gt (Nat 2) (Nat 1)) (Return (Nat 4)) Nop

  it "while" $ parseStmt "while ( 1<2 ) { return 3; };"
      `shouldBe` While (Lt (Nat 1) (Nat 2)) (Return (Nat 3))

  it "for" $ do
    parseStmt "for ( i=1; i<10; i+1 ) { return i; };"
      `shouldBe` For (Assign "i" (Nat 1)) (Lt (LVar "i") (Nat 10)) (Add (LVar "i") (Nat 1)) (Return (LVar "i"))
    parseStmt " for (;;) { return 1; };"
      `shouldBe` For Nop (Nat 1) (Nat 0) (Return (Nat 1))

