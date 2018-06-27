module Ast.Ast 
    ( Variable(..)
    , VariableExp(..)
    , Unop(..)
    , Binop(..)
    , Exp(..)
    , Stm(..)
    , Function(..)
    , Program(..)
    ) where

import Common

import Data.List (intercalate)

data Variable = Variable Id deriving (Eq)

data VariableExp =
      Var Variable
    | Arr VariableExp Exp
    | Deref VariableExp
    | AddrOf VariableExp
    deriving (Eq)

data Unop =
      Neg
    | Not
    deriving (Eq)

data Binop =
      Add
    | And
    | Div
    | Mul
    | Rem
    | Or
    | ShiftL
    | ShiftR
    | Sub
    | Xor
    | Cmp CmpOp
    deriving (Eq)

data Exp =
      Un Unop Exp
    | Bin Binop Exp Exp
    | Call Symbol [Exp]
    | Cast Width Exp
    | VarExp VariableExp
    | Imm Word64
    deriving (Eq)

data Stm =
      Mov VariableExp Exp
    | IfElse Exp [Stm] [Stm]
    | If Exp [Stm]
    | While Exp [Stm]
    | Return Exp
    deriving (Eq)

data Function = Func Symbol [Variable] [Stm] deriving (Eq)

data Program = Prog [Function] deriving (Eq)

instance Show Variable where
    show (Variable id) = "v" ++ show id

instance Show VariableExp where
    show (Var v) = show v
    show (Arr a i) = show a ++ "[" ++ show i ++ "]"
    show (Deref p) = "*" ++ show p
    show (AddrOf v) = "&" ++ show v

instance Show Unop where
    show Neg = "!"
    show Not = "~"

instance Show Binop where
    show Add = "+"
    show And = "&"
    show Div = "/"
    show Mul = "*"
    show Rem = "%"
    show Or = "|"
    show ShiftL = "<<"
    show ShiftR = ">>"
    show Sub = "-"
    show Xor = "^"
    show (Cmp op) = show op

instance Show Exp where
    show (Un op s) = show op ++ show s
    show (Bin op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
    show (Call f args) = show f ++ "(" ++ join ", " (map show args) ++ ")"
    show (Cast w s) = "(" ++ show w ++ ") " ++ show s
    show (VarExp v) = show v
    show (Imm i) = show i

join = intercalate

showStms d stms = join "\n" (map (showStm d) stms)

showStm d stm = case stm of
    Mov d s -> spcs ++ show d ++ " = " ++ show s ++ ";"
    IfElse cond tStms fStms ->
        spcs ++ "if (" ++ show cond ++ ") {\n" ++ showStms' tStms ++ "\n" ++
        spcs ++ "} else {\n" ++ showStms' fStms ++  "\n" ++
        spcs ++ "}"
    If cond stms ->
        spcs ++ "if (" ++ show cond ++ ") {\n" ++ showStms' stms ++ "\n" ++ spcs ++ "}"
    While cond stms ->
        spcs ++ "while (" ++ show cond ++ ") {\n" ++ showStms' stms ++ "\n" ++ spcs ++ "}"
    Return s -> spcs ++ "return " ++ show s ++ ";"
  where
    showStms' = showStms $ d + 1
    spcs = map (const ' ') [1..d*2]

instance Show Stm where
    show stm = showStm 0 stm

instance Show Function where
    show (Func id args stms) =
        "int " ++ show id ++ "(" ++ join ", " (map (\a -> "int " ++ show a) args) ++ ") {\n" ++ showStms 1 stms ++ "\n}"

instance Show Program where
    show (Prog funs) = join "\n\n" (map show funs)