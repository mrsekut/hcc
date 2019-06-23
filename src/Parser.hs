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
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Nat Int
            deriving Show

-- TODO: Either Monad
-- TODO: skip space
-- expr ::= term ('+' expr | '-' expr) *
expr :: Parser Expr
expr = do
    t <- term
    (Add t <$> (char '+' *> expr)) <|> (Sub t <$> (char '-' *> expr)) <|> pure t


-- term ::= factor ('*' term | '/' term) *
term :: Parser Expr
term = do
    f <- factor
    (Mul f <$> (char '*' *> term)) <|> (Div f <$> (char '/' *> term)) <|> pure f


-- factor ::= '(' expr ')' | nat
factor :: Parser Expr
factor = (char '(' *> expr <* char ')') <|> nat


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . digitToInt <$> oneOf ['0' .. '9']


-- run expr "1+2"
-- > Add (Nat 1) (Nat 2)
run :: Show a => Parser a -> String -> IO ()
run p input =
    case parse p "hoge" input of
        Left err -> putStr "parse error at" >> print err
        Right x -> print x


parseExpr :: String -> Either ParseError Expr
parseExpr s = parse expr "* ParseError *" s
