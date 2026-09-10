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
import Control.Applicative


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
evalLit e (LDictEmpty e1 e2) = Right . VDict <$> HM.empty Nothing defaultHash 
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
evalUnary env Neg e    = evalUnaryOp evalNegate env e 
evalUnary env Not e    = evalUnaryOp (\(VBool b) -> pure $ Right $ VBool $ not b) env e 
evalUnary env ToInt e  = evalUnaryOp evalToInt env e
evalUnary env ToReal e = evalUnaryOp evalToReal env e
evalUnary env Fact e   = evalUnaryOp evalFact env e
evalUnary env Len e    = evalUnaryOp evalLen env e
evalUnary env Head e   = evalUnaryOp evalHead env e
evalUnary env Tail e   = evalUnaryOp evalTail env e


evalUnaryOp :: (Val s -> ST s (Either Error (Val s))) -> Env s -> Expr -> ST s (Either Error (Val s))
evalUnaryOp op env e = do 
  re <- eval env e 
  either (pure . Left) op re  


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


evalBinary :: Env s -> Binary -> Expr -> Expr -> ST s (Either Error (Val s))
evalBinary env op e1 e2
  | op `elem` [Add, Sub, Mul] = evalBinaryOp (evalArithm $ getArithm op) env e1 e2 
  | op `elem` [Div, Pow]      = evalBinaryOp (evalArithmReal $ getArithmReal op) env e1 e2 
  | op `elem` [Mod, IntDiv]   = evalBinaryOp (evalArithmInt $ getArithmInt op) env e1 e2   
  | op `elem` [And, Or, Xor]  = evalBinaryOp (evalBool $ getBool op) env e1 e2 
  | otherwise                 = undefined


evalArithm :: ((Double -> Double -> Either Error Double), (Int -> Int -> Either Error Int)) -> Val s -> Val s -> ST s (Either Error (Val s))
evalArithm (_, g) (VInt x) (VInt y)   = pure $ VInt  <$> (g x y)
evalArithm (f, _) (VInt x) (VReal y)  = pure $ VReal <$> (f (fromIntegral x) y)
evalArithm (f, _) (VReal x) (VInt y)  = pure $ VReal <$> (f x (fromIntegral y))
evalArithm (f, _) (VReal x) (VReal y) = pure $ VReal <$> (f x y)
evalArithm _ _ _                      = undefined 


getArithm :: (Real a, Real b) => Binary -> ((a -> a -> Either Error a), (b -> b -> Either Error b))
getArithm Add = (liftBin (+), liftBin (+))
getArithm Sub = (liftBin (-), liftBin (-))
getArithm Mul = (liftBin (*), liftBin (*))
getArithm _   = undefined 


evalArithmReal :: (Double -> Double -> Either Error Double) -> Val s -> Val s -> ST s (Either Error (Val s))
evalArithmReal f (VInt x) (VInt y)   = pure $ VReal <$> (f (fromIntegral x) (fromIntegral y)) 
evalArithmReal f (VInt x) (VReal y)  = pure $ VReal <$> (f (fromIntegral x) y)
evalArithmReal f (VReal x) (VInt y)  = pure $ VReal <$> (f x (fromIntegral y))
evalArithmReal f (VReal x) (VReal y) = pure $ VReal <$> (f x y) 
evalArithmReal _ _ _                 = undefined 


getArithmReal :: Binary -> (Double -> Double -> Either Error Double)
getArithmReal Div                              = safeDiv 
  where
    safeDiv x 0                                = Left $ EvalError ""
    safeDiv x y                                = Right $ x / y 
getArithmReal Pow                              = safePow 
  where
    safePow x y 
      | x == 0 && y <= 0                       = Left $ EvalError ""
      | x < 0 && (x /= fromIntegral (round x)) = Left $ EvalError ""
      | otherwise                              = Right $ x ** y 
getArithmReal _                                = undefined 


evalArithmInt :: (Int -> Int -> Either Error Int) -> Val s -> Val s -> ST s (Either Error (Val s))
evalArithmInt f (VInt x) (VInt y) = pure $ VInt <$> f x y 
evalArithmInt f _ _               = undefined 


getArithmInt :: Binary -> (Int -> Int -> Either Error Int)
getArithmInt Mod    = safeMod 
  where
    safeMod m n 
      | n == 0      = Left $ EvalError ""
      | otherwise   = Right $ mod m n 
getArithmInt IntDiv = safeIntDiv 
  where
    safeIntDiv m n 
      | n == 0      = Left $ EvalError ""
      | otherwise   = Right $ div m n 


evalBool :: (Bool -> Bool -> Either Error Bool) -> Val s -> Val s -> ST s (Either Error (Val s))
evalBool f (VBool p) (VBool q) = pure $ VBool <$> f p q  
evalBool _ _ _                 = undefined 


getBool :: Binary -> (Bool -> Bool -> Either Error Bool)
getBool And = liftBin (&&)
getBool Or  = liftBin (||)
getBool Xor = liftBin (/=)


liftBin :: (a -> a -> a) -> (a -> a -> Either Error a)
liftBin f = \x y -> pure $ f x y


evalBinaryOp :: (Val s -> Val s -> ST s (Either Error (Val s))) -> Env s -> Expr -> Expr -> ST s (Either Error (Val s))
evalBinaryOp op env e1 e2 = do 
  re1 <- eval env e1 
  re2 <- eval env e2 
  case (re1, re2) of 
    err@(Left err1, Left err2) -> pure $ fst err <|> snd err 
    (Left err, Right _)        -> pure $ Left err
    (Right _, Left err)        -> pure $ Left err 
    (Right ve1, Right ve2)     -> op ve1 ve2  