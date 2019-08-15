module Main where

import           System.Environment             ( getArgs )
import           Parser                         ( parseExpr
                                                , expr
                                                , Expr(..)
                                                )
import           Text.Parsec

asmHeader = mapM_ putStrLn [".intel_syntax noprefix", ".global hcc", "hcc:"]


gen :: Expr -> IO ()
gen e = case e of
    Nat i -> putStrLn $ "    push " ++ show i
    Add e1 e2 -> do
        gen e1
        gen e2
        putStrLn "    pop rdi"
        putStrLn "    pop rax"
        putStrLn "    add rax, rdi"
        putStrLn "    push rax"
    Sub e1 e2 -> do
        gen e1
        gen e2
        putStrLn "    pop rdi"
        putStrLn "    pop rax"
        putStrLn "    sub rax, rdi"
        putStrLn "    push rax"
    Mul e1 e2 -> do
        gen e1
        gen e2
        putStrLn "    pop rdi"
        putStrLn "    pop rax"
        putStrLn "    imul rdi"
        putStrLn "    push rax"
    Div e1 e2 -> do
        gen e1
        gen e2
        putStrLn "    pop rdi"
        putStrLn "    pop rax"
        putStrLn "    cqo"
        putStrLn "    idiv rdi"
        putStrLn "    push rax"


main :: IO ()
main = do
    -- let p = Add (Nat 2) (Nat 3)
    -- gen p

    args <- getArgs
    case args of
        [] -> putStrLn "Incorrect number of arguments"
        _  -> do
            asmHeader
            case parseExpr (head args) of
                Right e -> gen e
                Left  e -> print e
            putStrLn $ "    pop rax"
            putStrLn $ "    ret"
