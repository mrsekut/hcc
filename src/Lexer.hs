module Lexer
    ( TokenType(..)
    , Token(..)
    , lexer
    )
where

cOPERATORS = ['+', '-']
cIDENTNUM = ['0' .. '9']

data TokenType = TK_NUM | TK_OP | TK_EOF deriving (Show, Eq)
data Token = Token {
    tokenType :: TokenType,
    valueString :: String
} deriving (Show, Eq)

-- newtype Row = Row Int
-- newtype Column = Column Int
-- type Pos = (Row, Column)
-- type PosToken = [(Token, (Pos, String))]


isOperator :: Char -> Bool
isOperator op = op `elem` cOPERATORS

isNumber :: Char -> Bool
isNumber x = x `elem` cIDENTNUM

tokenizeOperator :: String -> [Token]
tokenizeOperator []       = error "Lexer couldn't parse Operator Literal"
tokenizeOperator (x : xs) = Token TK_OP [x] : lexer xs

tokenizeNumber :: String -> String -> [Token]
tokenizeNumber [] _  = error "Lexer couldn't parse Number Literal"
tokenizeNumber t  [] = [Token TK_NUM t]
tokenizeNumber t (x : xs) | isNumber x = tokenizeNumber (t ++ [x]) xs
                          | otherwise  = Token TK_NUM t : lexer (x : xs)

lexer :: String -> [Token]
lexer [] = []
lexer (x : xs) | isOperator x = tokenizeOperator (x : xs)
               | isNumber x   = tokenizeNumber [x] xs
               | otherwise    = lexer xs


