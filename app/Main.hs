import System.Environment (getArgs)
-- module Main where
-- import Lexer

asmHeader = do
    mapM_ putStrLn $ [".intel_syntax noprefix", ".global main", "main:"]


main :: IO ()
main = do
    args <- getArgs
    if 1 == length args
    then do
        asmHeader
        putStrLn $ "    mov rax," ++ args !! 0
        putStrLn $ "    ret"
    else
        putStrLn $ "Incorrect number of arguments"
