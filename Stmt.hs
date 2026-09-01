module Stmt where 


import Expr 


type Program = [Stmt]


data Stmt
    = Assign String Expr 
    | Set Expr Expr Expr 
    | Declare String Expr 
    | While Expr Program 
    | If Expr Program Program 
    | Block [Expr] Program 
    | Print Expr 
    | PrintF String [Expr]
    | Input String String  