module Expr where 


import Utils 
import Data.Array.ST (STArray)
import GHC.Internal.TypeLits (Mod)


data Expr 
    = Lit Lit 
    | Var String Expr 
    | Unary Unary Expr 
    | Binary Binary Expr Expr
    | StrOp StrOp Expr Expr 
    | TupleOp TupleOp Expr Expr 
    | ListOp ListOp Expr Expr 
    | ArrayOp ArrayOp 
    | DictOp DictOp 
    | FunOp FunOp Expr Expr 
    | Let Expr Expr Expr 
    | If Expr Expr Expr 


data Lit 
    = LTrue 
    | LFalse 
    | LInt Int 
    | LReal Double 
    | LString String 
    | LList Expr 
    | LListEmpty [Expr]
    | LArray [Expr]
    | LArrayEmpty [Expr]
    | LTuple [Expr]
    | LDict [(Expr, Expr)]
    | LDictEmpty Expr Expr 
    | LArrow String Expr Expr


data Unary 
    = Not 
    | Negate 
    | Fact 
    | StrLen 
    | TupleLen 
    | ListLen 
    | ArrayLen 


data Binary 
    = Arithm Arithm 
    | ArithmInt ArithmInt 
    | BoolAlg BoolAlg 
    | Comp Comp 


data Arithm 
    = Add 
    | Sub 
    | Mul 
    | Div 
    | Pow 


data ArithmInt 
    = Mod
    | IntDiv 


data BoolAlg 
    = And 
    | Or 
    | Xor 


data Comp 
    = Eq 
    | NEq 
    | LEq 
    | GEq 
    | LTn 
    | GTn 


data StrOp 
    = StrConcat 
    | StrComp StrComp 
    | StrIndex 


data StrComp 
    = SEq 
    | SNEq 
    | SLEq 
    | SGEq 
    | SLTn 
    | SGTn 


data TupleOp 
    = TupleIndex 
    | TupleComp TupleComp 


data TupleComp 
    = TEq 
    | TNEq 
    | TLEq 
    | TGEq 
    | TLTn 
    | TGTn 


data ListOp 
    = Cons 
    | ListConcat 
    | ListComp ListComp 


data ListComp 
    = LEQ 
    | LNEq 
    | LLEq 
    | LGEq 
    | LLTn 
    | LGTn 


data ArrayOp 
    = ArraIndex Expr Expr 
    | ArraySet Expr Expr Expr 


data DictOp 
    = DictKey Expr Expr 
    | DictInsert Expr Expr Expr


data FunOp 
    = Apply 
    | Compose 