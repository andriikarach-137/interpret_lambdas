module TypeUtils where 


import Type 
import Error 
import Control.Applicative


data TypeClass 
    = Equatable 
    | Comparable 
    | Numeric 
    | Integral
    | BoolLike 
    | Collectable 
    | Inductive 
    deriving Eq


typeClass :: Type -> [TypeClass] 
typeClass TInt         = [Equatable, Comparable, Numeric, Integral] 
typeClass TReal        = [Equatable, Comparable, Numeric]
typeClass TBool        = [Equatable, BoolLike]
typeClass TString      = [Equatable, Comparable]
typeClass (TList t)    = [Equatable | hasClass Equatable t] ++ [Comparable | hasClass Comparable t] ++ [Inductive, Collectable]
typeClass (TArray t)   = [Collectable]
typeClass (TTuple ts)  = [Equatable | all (hasClass Equatable) ts] ++ [Comparable | all (hasClass Comparable) ts] ++ [Collectable]
typeClass (TDict t t') = [Collectable]


hasClass :: TypeClass -> Type -> Bool 
hasClass tc = (tc `elem`) . typeClass 


expectClass :: TypeClass -> Type -> Either TypeError ()
expectClass tc t = if tc `elem` typeClass t then Right () else Left $ TypeError ""


expectType :: Type -> Type -> Either TypeError ()
expectType t t' 
  | t == t'   = Right ()
  | otherwise = Left $ TypeError "" 


expectBoth :: TypeClass -> Type -> Type -> Either TypeError ()
expectBoth = liftA2 expectClass2 id id 


expectClass2 :: TypeClass -> TypeClass -> Type -> Type -> Either TypeError ()
expectClass2 tc tc' t t' = expectClass tc t *> expectClass tc' t' 


expectSame :: TypeClass -> Type -> Type -> Either TypeError ()
expectSame tc t t' = expectBoth tc t t' *> expectType t t' 


const2 :: a -> b -> a -> a 
const2 = \x _ _ -> x 


arithmResult :: Type -> Type -> Type 
arithmResult TInt TInt = TInt 
arithmResult _ _ = TReal 


expectCons :: Type -> Type -> Either TypeError ()
expectCons t (TList t') = expectType t t' 
expectCons _ _          = Left $ TypeError ""


expectGet :: Type -> Type -> Either TypeError ()
expectGet (TList t) TInt  = Right ()
expectGet (TArray t) TInt = Right ()
expectGet (TDict t _) t'  = expectType t t' 
expectGet _ _             = Left $ TypeError ""


getResult :: Type -> Type -> Type 
getResult (TList t) _  = t 
getResult (TArray t) _ = t
getResult _ _ = undefined 


expectApply :: Type -> Type -> Either TypeError ()
expectApply (TArrow t _) = expectType t 


expectCompose :: Type -> Type -> Either TypeError ()
expectCompose (TArrow t t') (TArrow t1 t1') = expectType t1' t 