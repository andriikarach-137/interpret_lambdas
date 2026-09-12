module Expr where 


data Expr 
    = Lit Lit 
    | Col Col 
    | Def Def 
    | Var String
    | Unary Unary Expr 
    | Binary Binary Expr Expr
    | Let String Expr Expr 
    | If Expr Expr Expr 
    | Arrow String Expr Expr
    deriving Eq 


data Lit 
    = LTrue 
    | LFalse 
    | LInt Int 
    | LReal Double 
    | LString String 
    deriving Eq 


data Col
    = CList [Expr]
    | CListEmpty Expr 
    | CArray [Expr] Int 
    | CTuple [Expr]
    | CDict [(Expr, Expr)]
    | CDictEmpty Expr Expr 
    deriving Eq 

data Def 
    = DBool 
    | DInt 
    | DReal 
    | DString 
    | DList Def 
    | DArray Def 
    | DTuple [Def]
    | DDict Def Def
    | DArrow Def Def 
    deriving Eq 


data Unary 
    = Not 
    | Neg
    | ToInt  
    | ToReal
    | Fact 
    | Len 
    | Head 
    | Tail  
    deriving Eq 


data Binary 
    = Add
    | Sub 
    | Mul 
    | Div 
    | Pow 
    | Mod 
    | IntDiv 
    | And 
    | Or 
    | Xor 
    | Eq 
    | NEq 
    | LEq 
    | GEq 
    | LTn 
    | GTn 
    | Concat 
    | Cons 
    | Get 
    | Project 
    | Apply 
    | Compose 
    deriving Eq 