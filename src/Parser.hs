module Parser
    ( expr
    , parseExpr
    , Expr(..)
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


-- TODO: 型をmodule化したい
data Expr = Add Expr Expr       -- 1 + 2
          | Sub Expr Expr       -- 1 - 2
          | Mul Expr Expr       -- 1 * 2
          | Div Expr Expr       -- 1 / 2
        --   | Plus Expr           -- +1
        --   | Minus Expr          -- -2
          | Nat Int             -- 1,2,..
            deriving Show

-- TODO: Either Monad
-- TODO: skip space
-- TODO: 2digits
-- expr ::= term ('+' expr | '-' expr) *
expr :: Parser Expr
expr = do
    t <- term
    (Add t <$> (char '+' *> expr)) <|> (Sub t <$> (char '-' *> expr)) <|> pure t


-- term ::= unary ('*' unary | '/' unary) *
term :: Parser Expr
term = do
    f <- unary
    (Mul f <$> (char '*' *> unary))
        <|> (Div f <$> (char '/' *> unary))
        <|> pure f


-- unary ::= ('+' | '-')? factor
unary :: Parser Expr
unary = (char '+' *> factor) <|> Sub (Nat 0) <$> (char '-' *> factor) <|> factor


-- factor ::= '(' expr ')' | nat
factor :: Parser Expr
factor = (char '(' *> expr <* char ')') <|> nat


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . digitToInt <$> oneOf ['0' .. '9']


parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "* ParseError *"


-- run expr "1+2"
-- > Add (Nat 1) (Nat 2)
run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "hoge" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x

