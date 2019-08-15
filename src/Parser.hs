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
          | Eq Expr Expr        -- 1 == 2
          | Neq Expr Expr       -- 1 == 2
          | Lt Expr Expr        -- 1 < 2
          | Gt Expr Expr        -- 1 > 2
          | Lte Expr Expr       -- 1 <= 2
          | Gte Expr Expr       -- 1 >= 2
          | Nat Int             -- 1,2,..
            deriving Show

-- TODO: Either Monad
-- TODO: 右結合になっている
-- expr ::= add
expr :: Parser Expr
expr = assign


-- assigns ::= equality | equality "=" assign
assign :: Parser Expr
assign = equality


-- equality ::= add | add ("==" relational | "!=" relatoinal)
equality :: Parser Expr
equality = do
    r <- spaces *> relational
    (Eq r <$> (spaces *> string "==" *> spaces *> equality))
        <|> (Neq r <$> (spaces *> string "!=" *> spaces *> equality))
        <|> pure r


-- relational ::= add | add ("<" add | "<=" add | ">" add | ">=" add)
relational :: Parser Expr
relational = do
    a <- spaces *> add
    (do
            spaces *> char '<'
            (Lt a <$> (spaces *> relational))
                <|> (Lte a <$> (char '=' *> spaces *> relational))
        )
        <|> (do
                spaces *> char '>'
                (Gt a <$> (spaces *> relational))
                    <|> (Gte a <$> (char '=' *> spaces *> relational))
            )
        <|> pure a


-- add ::= term | term ('+' add | "-" add)
add :: Parser Expr
add = do
    t <- spaces *> term
    (Add t <$> (spaces *> char '+' *> spaces *> add))
        <|> (Sub t <$> (spaces *> char '-' *> spaces *> add))
        <|> pure t


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
run p input = case parse p "hcc" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x

