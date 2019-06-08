module Main where

import           System.Environment             ( getArgs )
import           Lexer


asmHeader = mapM_ putStrLn [".intel_syntax noprefix", ".global main", "main:"]

compile :: [Token] -> IO ()
compile (x:xs) = do
    asmHeader
    putStrLn $ "  mov rax, " ++ valueString x
    mapM_ putStrLn $ c xs
    putStrLn "  ret"

c:: [Token] -> [String]
c [] = []
c (x:y:xs) = case tokenType x of
        TK_NUM -> c xs
        TK_OP -> case valueString x of
            "+" -> ("  add rax, " ++ valueString y) : c xs
            "-" -> ("  sub rax, " ++ valueString y) : c xs
        _ -> "error": c xs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Incorrect number of arguments"
        _  -> do
            let tokens = lexer $ head args
            compile tokens