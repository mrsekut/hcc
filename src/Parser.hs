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


data Expr = Add Expr Expr       -- 1 + 2
          | Sub Expr Expr       -- 1 - 2
          | Mul Expr Expr       -- 1 * 2
          | Div Expr Expr       -- 1 / 2
          | Nat Int             -- 1,2,..
            deriving Show

-- TODO: Either Monad
-- TODO: 右結合になっている

-- expr ::= add
expr :: Parser Expr
expr = add


-- add ::= term | term ('+' add | "-" add)
add :: Parser Expr
add = do
    t <- spaces *> term
    (Add t <$> (spaces *> char '+' *> spaces *> add))
        <|> (Sub t <$> (spaces *> char '-' *> spaces *> add))
        <|> pure t


-- -- assigns ::=
-- assign :: Parser Expr
-- assinn = undefined


-- term ::= unary | unary ('*' unary |'/' unary)
term :: Parser Expr
term = do
    u <- spaces *> unary
    (Mul u <$> (spaces *> char '*' *> spaces *> term))
        <|> (Div u <$> (spaces *> char '/' *> spaces *> term))
        <|> pure u


-- unary ::= factor | ('+' | '-') factor
unary :: Parser Expr
unary =
    (spaces *> char '+' *> factor)
        <|> Sub (Nat 0)
        <$> (spaces *> char '-' *> factor)
        <|> factor


-- factor ::= '(' expr ')' | nat
factor :: Parser Expr
factor =
    (spaces *> char '(' *> spaces *> expr <* spaces <* char ')' <* spaces)
        <|> spaces
        *>  nat
        <*  spaces


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . read <$> many1 digit


parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "* ParseError *"


-- run expr "1+2"
-- > Add (Nat 1) (Nat 2)
run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "hoge" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x

