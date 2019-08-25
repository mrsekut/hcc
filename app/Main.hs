module Main where

import           System.Environment             ( getArgs )
import           Parser                         ( parseProgram
                                                , expr
                                                , Expr(..)
                                                )
import           Text.Parsec

asmHeader = mapM_ putStrLn [".intel_syntax noprefix", ".global hcc", "hcc:"]


class Reifiable a where
    reify :: a -> [ String ]

instance Reifiable Expr where
    reify (Nat i) = ["    push " ++ show i]
    reify (Add e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    add rax, rdi"
               , "    push rax"
               ]
    reify (Sub e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    sub rax, rdi"
               , "    push rax"
               ]
    reify (Mul e1 e2) =
        reify e1
            ++ reify e2
            ++ ["    pop rdi", "    pop rax", "    imul rdi", "    push rax"]
    reify (Div e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cqo"
               , "    idiv rdi"
               , "    push rax"
               ]
    reify (Eq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    sete al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Neq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setne al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Lt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setl al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Lte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setle al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Gt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setg al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (Gte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setge al"
               , "    movzb rax, al"
               , "    push rax"
               ]


gen :: Expr -> IO ()
gen = mapM_ putStrLn . reify


main :: IO ()
main = do
    -- let p = Add (Nat 2) (Nat 3)
    -- gen p

    args <- getArgs
    case args of
        [] -> putStrLn "Incorrect number of arguments"
        _  -> do
            asmHeader
            case parseProgram (head args) of
                Right e -> gen e
                Left  e -> print e
            putStrLn $ "    pop rax"
            putStrLn $ "    ret"
