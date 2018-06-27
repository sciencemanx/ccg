module Tree.Insn 
    ( Variable(..)
    , VariableExp(..)
    , Unop(..)
    , Binop(..)
    , Exp(..)
    , Stm(..)
    ) where

import Common

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
    | Goto Addr
    | GotoIf Exp Addr
    | Return Exp
    deriving (Eq)

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
    show (Call f args) = "fun_" ++ show f ++ "(args)"
    show (Cast w s) = "(" ++ show w ++ ") " ++ show s
    show (VarExp v) = show v
    show (Imm i) = show i

instance Show Stm where
    show (Mov d s) = show d ++ " = " ++ show s ++ ";"
    show (Goto t) = "goto " ++ show t ++ ";"
    show (GotoIf cond t) = "if (" ++ show cond ++ ") goto " ++ show t ++ ";" 
    show (Return s) = "return " ++ show s ++ ";"
