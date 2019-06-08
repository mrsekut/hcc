module Main where

import           System.Environment             ( getArgs )
import           Lexer (lexer)

-- isFirstTokenNumber :: [Token] -> Bool
-- isFirstTokenNumber = undefined

asmHeader = do
    mapM_ putStrLn $ [".intel_syntax noprefix", ".global main", "main:"]

-- compile :: [Token] -> IO ()
-- compile token = do
--     asmHeader




main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn $ "Incorrect number of arguments"
        _  -> do
            asmHeader
            -- tokens <- lexer args !! 0
            -- -- compile tokens
            -- putStrLn $ "    mov rax, " ++ args !! 0
            print $ lexer "3+3"

