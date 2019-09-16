module Main where

import           System.Environment             ( getArgs )
import           Parser                         ( parseProgram
                                                , expr
                                                , Expr(..)
                                                , Program(..)
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

instance Reifiable Stmt where
    reify (S s) = reify =<< s
    reify (Assign v e) =
        ["    mov rax, rbp", "    sub rax, " ++ offset v, "    push rax"]
            ++ reify e
            ++ [ "    pop rdi"
               , "    pop rax"
               , "    mov [rax], rdi"
               , "    push rdi"
               ]
    reify (Return e) = reify e ++
                       [ "    pop rax"
                       , "    mov rsp, rbp"
                       , "    pop rbp"
                       , "    ret"
                       ]
    reify (If b s1 s2) = case s2 of
        Nop -> reify b ++
                       [ "    pop rax"
                       , "    cmp rax, 0"
                       , "    je .LendXXX" -- FIXME:
                       ] ++ reify s1 ++
                       [ ".LendXXX:"]
        _ -> reify b ++
                       [ "    pop rax"
                       , "    cmp rax, 0"
                       , "    je .LelseXXX" -- FIXME:
                       ] ++ reify s1 ++
                       [ "    jmp .LendXXX"
                       , ".LelseXXX:"
                       ] ++ reify s2 ++
                       [ ".LendXXX:"]
    reify (While e s) =  [".LbeginXXX:"]
                       ++ reify e ++
                       [ "    pop rax"
                       , "    cmp rax, 0"
                       , "    je  .LendXXX"
                       ] ++ reify s ++
                       [ "    jmp .LbeginXXX"
                       , ".LendXXX:"
                       ]
    reify (For pre cond post body) = reify pre ++ -- TODO: Nop, 1, 0のときの処理
                              [".LbeginXXX:"]
                              ++ reify cond ++
                              [ "    pop rax"
                              , "    cmp rax, 0"
                              , "    je  .LendXXX"
                              ] ++ reify body ++ reify post ++
                              [ "    jmp  .LbeginXXX"
                              , ".LendXXX:"]


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
