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
import           Data.Functor.Identity


-- TODO: 型の分割をしたい
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


-- expr ::= add
expr :: Parser Expr
expr = assign


-- assigns ::= equality | equality "=" assign
assign :: Parser Expr
assign = equality


-- equality ::= add | add ("==" relational | "!=" relatoinal)
equality :: Parser Expr
equality = relational `chainl1` equalityop

equalityop :: Parser (Expr -> Expr -> Expr)
equalityop = choice $ map try [Neq <$ string "!=", Eq <$ string "=="]


-- relational ::= add | add ("<" add | "<=" add | ">" add | ">=" add)
relational :: Parser Expr
relational = add `chainl1` relop

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


-- factor ::= '(' expr ')' | nat
factor :: Parser Expr
factor =
    between (spaces *> char '(' <* spaces) (spaces *> char ')' <* spaces) expr
        <|> skipW nat


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . read <$> many1 digit


-- utils ------------------------

skipW :: Parser Expr -> Parser Expr
skipW p = spaces *> p <* spaces

skipW1
    :: Parser (Expr -> Expr -> Expr)
    -> ParsecT String () Identity (Expr -> Expr -> Expr)
skipW1 f = spaces *> f <* spaces

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "* ParseError *"


-- run expr "1+2"
-- > Add (Nat 1) (Nat 2)
run :: Show a => Parser a -> String -> IO ()
run p input = case parse p "hcc" input of
    Left  err -> putStr "parse error at" >> print err
    Right x   -> print x

