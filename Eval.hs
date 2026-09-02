module Eval where 


import Data.Array.ST(STArray)
import Data.Array.ST qualified as A 
import Control.Monad.ST 

import Data.Map (Map)
import Data.Map qualified as Map 


import Expr 
import Error 
import Type 
import Control.Monad (forM_)


type Env s = Map String (Val s)


data Val s
  = VNothing 
  | VBool Bool 
  | VInt Int 
  | VReal Double 
  | VString (STArray s Int Char)
  | VList [Val s]
  | VArray (STArray s Int (Val s))
  | VTuple (STArray s Int (Val s))
  | VDict (STArray s Int (Val s, Val s))
  | VArrow (Maybe (Env s)) String Expr 


eval :: Env s -> Expr -> ST s (Either Error (Val s))
eval env (Lit l) = evalLit env l 


evalLit :: Env s -> Lit -> ST s (Either Error (Val s))
evalLit _ LTrue              = pure $ Right $ VBool True 
evalLit _ LFalse             = pure $ Right $ VBool False 
evalLit _ (LInt n)           = pure $ Right $ VInt n
evalLit _ (LReal x)          = pure $ Right $ VReal x 
evalLit _ (LString str)      = Right <$> VString <$> A.newListArray (0, length str - 1) str 
evalLit e (LList l)          = evalList e l  
evalLit _ (LListEmpty _)     = pure $ Right $ VList []
evalLit e (LArray a c)       = evalArray e a c 
evalLit e (LTuple a)         = evalTuple e a 
evalLit e (LDict ps)         = undefined 
evalLit e (LDictEmpty e1 e2) = undefined 
evalLit e (LArrow s e1 e2)   = undefined 


evalList :: Env s -> [Expr] -> ST s (Either Error (Val s))
evalList e l = (fmap VList) . sequence <$> traverse (eval e) l 


evalArrVals :: Env s -> [Expr] -> ST s (Either Error [Val s])
evalArrVals e l = sequence <$> traverse (eval e) l 


evalArray :: Env s -> [Expr] -> Int -> ST s (Either Error (Val s))
evalArray e l c
  | c <= 0 || c < length l = pure $ Left $ EvalError ""
  | otherwise              = do 
    arr     <- A.newArray (0, c - 1) VNothing :: ST s (STArray s Int (Val s))
    eitherL <- evalArrVals e l 
    case eitherL of
      Right l' -> do 
        forM_ (zip [0..] l') (\(i, v) -> A.writeArray arr i v)
        pure $ Right $ VArray arr 
      Left _ -> pure $ Left $ EvalError ""


evalTuple :: Env s -> [Expr] -> ST s (Either Error (Val s))
evalTuple e l = do 
  eitherVs <- evalArrVals e l 
  case eitherVs of 
    Right l' -> Right <$> VTuple <$> A.newListArray (0, length l' - 1) l' 
    Left _   -> pure $ Left $ EvalError ""