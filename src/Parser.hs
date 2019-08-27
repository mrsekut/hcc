{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( expr
    , parseProgram
    , Expr(..)
    , Stmt(..)
    )
where

import           Text.Parsec
import           Control.Applicative            ( (<$>)
                                                , (*>)
                                                , (<*)
                                                , pure
                                                )
import           Text.Parsec.String
import           Data.Char                      ( digitToInt )
import           Data.Functor.Identity


-- TODO: 型の分割をしたい
type Name = String
data Stmt = Stmt [Expr]
        --   | Assign Name Expr
          deriving Show
data Expr = Add Expr Expr       -- 1 + 2
          | Sub Expr Expr       -- 1 - 2
          | Mul Expr Expr       -- 1 * 2
          | Div Expr Expr       -- 1 / 2
          | Eq Expr Expr        -- 1 == 2
          | Neq Expr Expr       -- 1 == 2
          | Lt Expr Expr        -- 1 < 2
          | Gt Expr Expr        -- 1 > 2
          | Lte Expr Expr       -- 1 <= 2
          | Gte Expr Expr       -- 1 >= 2
          | Nat Int             -- 1,2,..
          | LVar Name           -- local variable
          | Assign Name Expr    -- hoge = 42 TODO: exprではなくない？
            deriving Show

-- prgoram ::= stmt*
program :: Parser Stmt
program = Stmt <$> many1 stmt

-- stmt ::= expr ";"
stmt :: Parser Expr
stmt = expr <* semi


-- expr ::= add
expr :: Parser Expr
expr = assign


-- TODO: 結合確認
-- assigns ::= equality | equality "=" assign
assign :: Parser Expr
assign = do
    e <- skipW equality
    Assign "x" <$> (char '=' *> skipW assign) <|> pure e

-- assign = equality `chainl1` skipW1 assignop

-- assignop :: Parser (Expr -> Expr -> Expr)
-- assignop = Assign <$ char '='


-- equality ::= add | add ("==" relational | "!=" relatoinal)
equality :: Parser Expr
equality = relational `chainl1` skipW1 equalityop

equalityop :: Parser (Expr -> Expr -> Expr)
equalityop = choice $ map try [Neq <$ string "!=", Eq <$ string "=="]


-- relational ::= add | add ("<" add | "<=" add | ">" add | ">=" add)
relational :: Parser Expr
relational = add `chainl1` skipW1 relop

relop :: Parser (Expr -> Expr -> Expr)
relop = choice $ map
    try
    [Lte <$ string "<=", Lt <$ string "<", Gte <$ string ">=", Gt <$ string ">"]


-- add ::= term | term ('+' add | "-" add)
add :: Parser Expr
add = term `chainl1` skipW1 addop

addop :: Parser (Expr -> Expr -> Expr)
addop = Add <$ char '+' <|> Sub <$ char '-'


-- term ::= unary | unary ('*' unary |'/' unary)
term :: Parser Expr
term = unary `chainl1` skipW1 termop

termop :: Parser (Expr -> Expr -> Expr)
termop = Mul <$ char '*' <|> Div <$ char '/'


-- unary ::= factor | ('+' | '-') factor
unary :: Parser Expr
unary =
    (spaces *> char '+' *> factor)
        <|> Sub (Nat 0)
        <$> (spaces *> char '-' *> factor)
        <|> factor


-- factor ::= nat | ident | "(" expr ")"
factor :: Parser Expr
factor =
    between (spaces *> char '(' <* spaces) (spaces *> char ')' <* spaces) expr
        <|> skipW nat
        <|> skipW var


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . read <$> many1 digit

var :: Parser Expr
var = LVar <$> many1 letter


-- char ------------------------------

semi :: Stream s m Char => ParsecT s u m Char
semi = char ';'


-- utils ------------------------

skipW :: Parser Expr -> Parser Expr
skipW p = spaces *> p <* spaces

skipW1
    :: Parser (Expr -> Expr -> Expr)
    -> ParsecT String () Identity (Expr -> Expr -> Expr)
skipW1 f = spaces *> f <* spaces


-- parse -------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "* ParseError *"

parseProgram :: String -> Either ParseError Stmt
parseProgram = parse program "* ParseError *"


-- debug -------------------------------

-- $ run expr "1+2"
-- > Add (Nat 1) (Nat 2)
run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "hcc" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x


-- $ run "1+2"
-- > Add (Nat 1) (Nat 2)
rune :: String -> IO ()
rune input = case parse expr "hcc" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x


-- $ runp "1+2;4;"
-- Add (Nat 1) (Nat 2)
-- Nat 4
runp :: String -> IO ()
runp input = case parse program "hcc" input of
    Left  err       -> putStr "parse error at" >> print err
    Right (Stmt st) -> mapM_ print st
