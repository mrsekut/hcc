{-# LANGUAGE FlexibleContexts #-}
module Parser
    ( expr
    , stmt
    , parseProgram
    , program
    , Program(..)
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
import qualified Data.Map                      as M


type Name = String

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

          | Eq  Expr Expr
          | Neq Expr Expr

          | Lt  Expr Expr
          | Lte Expr Expr
          | Gt  Expr Expr
          | Gte Expr Expr

          | Nat Int
          | LVar Name
            deriving (Show, Eq)


data Stmt = S [Expr]
          | Return Expr
          | Assign Name Expr
          | If Expr Stmt Stmt
        --   | While Expr Stmt
        --   | For Expr Expr Expr Stmt
          | Nop
          deriving (Show, Eq)

newtype Program = Program [Stmt] deriving (Show, Eq)


-- prgoram ::= stmt*
program :: Parser Program
program = Program <$> many1 stmt


-- stmt ::= assigns ";" | expr ";" | "return" expr ";"
stmt :: Parser Stmt
stmt = spaces *> choice (map try [assign, return', if', exprs]) <* semi

exprs :: Parser Stmt
exprs = S <$> many1 expr

return' :: Parser Stmt
return' = Return <$> (string "return" *> many1 (char ' ') *> expr)

if' :: Parser Stmt
if' = do
    string "if"
    b  <- parens expr
    s1 <- braces stmt
    s2 <- (string "else" *> braces stmt) <|> return Nop
    return $ If b s1 s2


-- assigns ::= ident | ident "=" assign
assign :: Parser Stmt
assign = do
    e <- many1 letter
    Assign e <$> (spaces *> char '=' *> skipW expr)


-- expr ::= equality
expr :: Parser Expr
expr = equality


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
-- addop = Add <$ char '+'


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
        <|> skipW ident


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . read <$> many1 digit

-- ident
ident :: Parser Expr
ident = LVar <$> many1 letter

-- char ------------------------------

semi :: Stream s m Char => ParsecT s u m Char
semi = char ';'


-- utils ------------------------

parens :: Parser a -> Parser a
parens p = spaces *> char '(' *> spaces *> p <* spaces <* char ')' <* spaces

braces :: Parser a -> Parser a
braces p = spaces *> char '{' *> spaces *> p <* spaces <* char '}' <* spaces

skipW :: Parser Expr -> Parser Expr
skipW p = spaces *> p <* spaces

skipW1
    :: Parser (Expr -> Expr -> Expr)
    -> ParsecT String () Identity (Expr -> Expr -> Expr)
skipW1 f = spaces *> f <* spaces


-- parse -------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "* ParseError *"

parseProgram :: String -> Either ParseError Program
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
    Left  err          -> putStr "parse error at" >> print err
    Right (Program pr) -> mapM_ print pr
