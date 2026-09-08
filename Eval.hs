module Eval where 


import Data.Array.ST(STArray)
import Data.Array.ST qualified as STA 

import Data.Array(Array)
import Data.Array qualified as A 

import Control.Monad.ST 

import Data.Map (Map)
import Data.Map qualified as Map 

import Expr 
import Error
import Type 
import Control.Monad (forM_)
import Value 
import HM 
import Data.STRef 


eval :: Env s -> Expr -> ST s (Either Error (Val s))
eval env (Lit l)      = evalLit env l 
eval env (Def d)      = error "Cannot evaluate default values"
eval env (Var x)      = case Map.lookup x env of 
  Just y             -> pure $ Right y 
  Nothing            -> pure $ Left $ EvalError ""
eval env (Unary op e) = evalUnary env op e   


evalLit :: Env s -> Lit -> ST s (Either Error (Val s))
evalLit _ LTrue              = pure $ Right $ VBool True 
evalLit _ LFalse             = pure $ Right $ VBool False 
evalLit _ (LInt n)           = pure $ Right $ VInt n
evalLit _ (LReal x)          = pure $ Right $ VReal x 
evalLit _ (LString str)      = pure $ Right $ VString $ A.listArray (0, length str - 1) str 
evalLit e (LList l)          = evalList e l  
evalLit _ (LListEmpty _)     = pure $ Right $ VList []
evalLit e (LArray a c)       = evalArray e a c 
evalLit e (LTuple a)         = evalTuple e a 
evalLit e (LDict ps)         = evalDict e ps 
evalLit e (LDictEmpty e1 e2) = Right . VDict <$> empty Nothing defaultHash 
evalLit _ (LArrow s _ e2)    = pure $ Right $ VArrow s e2   


evalList :: Env s -> [Expr] -> ST s (Either Error (Val s))
evalList e l = (fmap VList) . sequence <$> traverse (eval e) l 


evalArrVals :: Env s -> [Expr] -> ST s (Either Error [Val s])
evalArrVals e l = sequence <$> traverse (eval e) l 


evalArray :: Env s -> [Expr] -> Int -> ST s (Either Error (Val s))
evalArray e l c
  | c <= 0 || c < length l = pure $ Left $ EvalError ""
  | otherwise              = do 
    arr     <- STA.newArray (0, c - 1) VNothing
    eitherL <- evalArrVals e l 
    case eitherL of
      Right l' -> do 
        forM_ (zip [0..] l') (\(i, v) -> STA.writeArray arr i v)
        pure $ Right $ VArray (MutArr (length l) arr)
      Left _ -> pure $ Left $ EvalError ""


evalTuple :: Env s -> [Expr] -> ST s (Either Error (Val s))
evalTuple e l = do 
  eitherVs <- evalArrVals e l 
  case eitherVs of 
    Right l' -> pure $ Right $ VTuple $ A.listArray (0, length l' - 1) l' 
    Left _   -> pure $ Left $ EvalError ""


evalDict :: Env s -> [(Expr, Expr)] -> ST s (Either Error (Val s))
evalDict e ps = do 
  let (ks, vs) = unzip ps 
  ks' <- traverse (eval e) ks 
  vs' <- traverse (eval e) vs 
  let (ks'', vs'') = (sequence ks', sequence vs') 
  let ps' = liftA2 zip ks'' vs'' 
  case ps' of 
    Right ps'' -> Right . VDict <$> fromList defaultHash ps'' 
    _          -> pure $ Left $ EvalError ""


evalUnary :: Env s -> Unary -> Expr -> ST s (Either Error (Val s))
evalUnary env Neg e    = (eval env e) >>= either (pure . Left) evalNegate
evalUnary env Not e    = (eval env e) >>= (\re -> pure $ re >>= (\(VBool b) -> Right $ VBool $ not b))  
evalUnary env ToInt e  = (eval env e) >>= either (pure . Left) evalToInt
evalUnary env ToReal e = (eval env e) >>= either (pure . Left) evalToReal 
evalUnary env Fact e   = (eval env e) >>= either (pure . Left) evalFact 
evalUnary env Len e    = (eval env e) >>= either (pure . Left) evalLen 
evalUnary env Head e   = (eval env e) >>= either (pure . Left) evalHead 
evalUnary env Tail e   = (eval env e) >>= either (pure . Left) evalTail 


evalNegate :: Val s -> ST s (Either Error (Val s))
evalNegate (VInt n)  = pure $ Right $ VInt $ negate n 
evalNegate (VReal x) = pure $ Right $ VReal $ negate x 
evalNegate _         = undefined 


evalToInt :: Val s -> ST s (Either Error (Val s))
evalToInt n@(VInt _) = pure $ Right n 
evalToInt (VReal x)  = pure $ Right $ VInt $ truncate x 
evalToInt _          = undefined 


evalToReal :: Val s -> ST s (Either Error (Val s))
evalToReal (VInt n)    = pure $ Right $ VReal $ fromIntegral n 
evalToReal x@(VReal _) = pure $ Right x 
evalToReal _           = undefined 


evalFact :: Val s -> ST s (Either Error (Val s))
evalFact (VInt n)
  | n < 0     = pure $ Left $ EvalError ""
  | otherwise = pure $ Right $ VInt $ product [1..n]
evalFact _    = undefined 


evalLen :: Val s -> ST s (Either Error (Val s))
evalLen (VString a) = pure $ Right $ VInt $ length a 
evalLen (VList l)   = pure $ Right $ VInt $ length l  
evalLen (VArray a)  = pure $ Right $ VInt $ Value.size a  
evalLen (VTuple a)  = pure $ Right $ VInt $ length a 
evalLen (VDict d)   = readSTRef (HM.size d) >>= (\n -> pure $ Right $ VInt n) 
evalLen _           = undefined 


evalHead :: Val s -> ST s (Either Error (Val s))
evalHead (VList [])    = pure $ Left $ EvalError ""
evalHead (VList (x:_)) = pure $ Right $ x
evalHead _             = undefined 


evalTail :: Val s -> ST s (Either Error (Val s))
evalTail (VList [])     = pure $ Right $ VList []
evalTail (VList (_:xs)) = pure $ Right $ VList xs 
evalTail _              = undefined 