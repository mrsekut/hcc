module Main where

import           System.Environment             ( getArgs )
import           Parser                         ( parseProgram
                                                , expr
                                                , Expr(..)
                                                , Program(..)
                                                , BinOp(..)
                                                , Stmt(..)
                                                )
import           Text.Parsec
import           Data.Char
import           Control.Monad

newLinePrint :: [String] -> IO ()
newLinePrint = mapM_ putStrLn

asmHeader = newLinePrint [".intel_syntax noprefix", ".global hcc", "hcc:"]
prologue =
    newLinePrint ["    push rbp", "    mov rbp, rsp", "    sub rsp, 208"]
epilogue = newLinePrint ["    mov rsp, rbp", "    pop rbp", "    ret"]

offset :: String -> String
offset s = show $ (ord (head s) - ord 'a' + 1) * 8

class Reifiable a where
    reify :: a -> [ String ]


instance Reifiable Program where
    reify (Program ss) = ["undefined"] -- TODO:
    -- reify (Program ss) = reify =<< ss
    -- reify (Program ss) = join $ fmap reify ss

instance Reifiable Stmt where
    -- reify (S s) = ["hoge"]
    reify (S s) = reify =<< s
    reify (Assign v e) =
        ["    mov rax, rbp", "    sub rax, " ++ offset v, "    push rax"]
            ++ reify e
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    mov [rax], rdi"
               , "    push rdi"
               ]


instance Reifiable Expr where
    reify (Nat i) = ["    push " ++ show i]
    reify (LVar n) =
        [ "    mov rax, rbp"
        , "    sub rax, " ++ offset n
        , "    push rax"
        , "    pop rax"
        , "    mov rax, [rax]"
        , "    push rax"
        ]
    reify (B Add e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    add rax, rdi"
               , "    push rax"
               ]
    reify (B Sub e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    sub rax, rdi"
               , "    push rax"
               ]
    reify (B Mul e1 e2) =
        reify e1
            ++ reify e2
            ++ ["    pop rdi", "    pop rax", "    imul rdi", "    push rax"]
    reify (B Div e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cqo"
               , "    idiv rdi"
               , "    push rax"
               ]
    reify (B Eq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    sete al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (B Neq e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setne al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (B Lt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setl al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (B Lte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setle al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (B Gt e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setg al"
               , "    movzb rax, al"
               , "    push rax"
               ]
    reify (B Gte e1 e2) =
        reify e1
            ++ reify e2
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    cmp rax, rdi"
               , "    setge al"
               , "    movzb rax, al"
               , "    push rax"
               ]


gen :: Stmt -> IO ()
gen = mapM_ putStrLn . reify


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Incorrect number of arguments"
        _  -> do
            asmHeader
            prologue
            case parseProgram (head args) of
                Right (Program pr) -> mapM_ gen pr
                Left  e            -> print e
            epilogue
