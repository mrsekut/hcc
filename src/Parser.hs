module Parser (expr) where

import           Text.Parsec
import           Control.Applicative            ( (<$>)
                                                , (*>)
                                                , (<*)
                                                , pure
                                                )
import           Text.Parsec.String
import           Data.Char                      ( digitToInt )

data Expr = Add Expr Expr | Mul Expr Expr | Nat Int deriving Show

-- expr ::= term ('+' expr | term)
expr :: Parser Expr
expr = do
    t <- term
    (Add t <$> (char '+' *> expr)) <|> pure t


-- term ::= factor ('*' term | factor)
term :: Parser Expr
term = do
    f <- factor
    (Mul f <$> (char '*' *> term)) <|> pure f


-- factor ::= '(' expr ')' | nat
factor :: Parser Expr
factor = (char '(' *> expr <* char ')') <|> nat


-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Expr
nat = Nat . digitToInt <$> oneOf ['0' .. '9']

-- parseTest expr "1+2"
-- > Add (Nat 1) (Nat 2)



-- Parser -----------


-- -- data Expr = E_Int Int | E_Neg Expr | E_Sum Expr Expr | E_Prod Expr Expr | E_Var Char
-- --                       | E_Let {letvar::Char,letequal:: Expr,letin::Expr} deriving Show

-- data Expr = Mul | Add Mul Mul | Sub Mul Mul
-- -- -- expr = mul ("+" mul | "-" mul)*
-- expr :: [Token] -> (Expr, [Token])
-- expr []           = error "unexpected end of input"
-- expr (x : y : xs) = undefined

-- data Mul = Term | Mul Term Term | Div Term Term
-- -- mul  = term ("*" term | "/" term)*
-- mul :: [Token] -> (Mul, [Token])
-- mul []       = error "unexpected end of input"
-- mul (x : xs) = undefined


-- data Term = Leaf Num | Expr
-- -- term = num | "(" expr ")"
-- term :: [Token] -> (Term, [Token])
-- term [] = error "unexpected end of input"
-- term (x : xs) | x == '(' = expr xs
--               | Leaf x   = x

-- data Ast = Ast
-- parser :: [Token] -> Ast
-- parser []       = error "unexpected end of input"
-- parser (x : xs) = undefined







-- main = do
--     -- print $ lexer "3+3"
--     -- print $ lexer "3333+33"
--     compile ([ Token { tokenType = TK_NUM, valueString = "3" }
--                     , Token { tokenType = TK_OP, valueString = "+" }
--                     , Token { tokenType = TK_NUM, valueString = "3" }
--                     ]
--                    )
