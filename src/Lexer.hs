-- module Lexer where
module Lexer
    ( TokenType(..)
    , Token(..)
    , lexer
    )
where

c_OPERATORS = ['+', '-']
c_IDENT_NUM = ['0' .. '9']

data TokenType = TK_NUM | TK_OP | TK_EOF deriving (Show, Eq)
data Token = Token {
    tokenType :: TokenType,
    valueString :: String
} deriving (Show, Eq)

-- data Error loc msg = {
-- }

isOperator :: Char -> Bool
isOperator op = elem op c_OPERATORS

isNumber :: Char -> Bool
isNumber x = elem x c_IDENT_NUM

tokenizeOperator :: String -> [Token]
tokenizeOperator []       = error "Lexer couldn't parse Operator Literal"
tokenizeOperator (x : xs) = (Token TK_OP [x]) : lexer xs

-- 1,12,123,..
tokenizeNumber :: String -> String -> [Token]
tokenizeNumber [] _  = error "Lexer couldn't parse Number Literal"
tokenizeNumber t  [] = [(Token TK_NUM t)]
tokenizeNumber t (x : xs) | isNumber x = tokenizeNumber (t ++ [x]) xs
                          | otherwise  = (Token TK_NUM t) : lexer (x : xs)

lexer :: String -> [Token]
lexer [] = []
lexer (x : xs) | isOperator x = tokenizeOperator (x : xs)
               | isNumber x   = tokenizeNumber [x] xs
               | otherwise    = lexer xs

main = do
    -- print $ lexer "1+3-"
    print $ lexer "3+3"
    print $ lexer "3333+33"
    -- print $ lexer "33333+3"

