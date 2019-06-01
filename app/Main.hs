import System.Environment (getArgs)
-- module Main where
-- import Lib

main :: IO ()
main = do
    args <- getArgs
    if 1 == length args
    then do
        putStrLn $ ".intel_syntax noprefix"
        putStrLn $ ".global main"
        putStrLn $ "main:"
        putStrLn $ "    mov rax," ++ args !! 0
        putStrLn $ "    ret"
    else
        putStrLn $ "Incorrect number of arguments"
