module Eval where 


import Data.Array.ST(STArray)
import Data.Array.ST qualified as A 
import Control.Monad.ST 

import Data.Map (Map)
import Data.Map qualified as Map 


import Expr 
import Error 
import Type 


type Env s = Map String (Val s)


data Val s
  = VBool Bool 
  | VInt Int 
  | VReal Double 
  | VString (STArray s Int Char)
  | VList [Val s]
  | VArray (STArray s Int (Val s))
  | VTuple (STArray s Int (Val s))
  | VDict (STArray s Int (Val s, Val s))
  | VArrow (Maybe (Env s)) String Expr 


eval :: Env s -> Expr -> ST s (Val s) 
eval env (Lit l) = evalLit env l 


evalLit :: Env s -> Lit -> ST s (Val s)
evalLit e LTrue = pure $ VBool True 
evalLit e LFalse = pure $ VBool False 
evalLit e (LInt n) = pure $ VInt n
evalLit e (LReal x) = pure $ VReal x 
evalLit e (LString s) = VString <$> A.newListArray (0, length s - 1) s 
evalLit e (LList l) = VList <$> traverse (eval e) l  
evalLit e (LListEmpty _) = pure $ VList []
evalLit e (LArray a) = VArray <$> (traverse (eval e) a >>= (\a' -> A.newListArray (0, length a - 1) a'))
evalLit e (LTuple)