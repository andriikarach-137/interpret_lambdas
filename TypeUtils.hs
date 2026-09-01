module TypeUtils where 


import Type 
import Error 
import Expr 
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


unaryType :: Unary -> (Type -> Either Error (), Type -> Type)
unaryType Not    = (expectClass BoolLike, const TBool)
unaryType Neg    = (expectClass Numeric, id)
unaryType ToInt  = (expectClass Numeric, const TInt)
unaryType ToReal = (expectClass Numeric, const TReal) 
unaryType Fact   = (expectClass Integral, const TInt)
unaryType Len    = (expectClass Collectable, const TInt)
unaryType Head   = (expectClass Inductive, \(TList t) -> t)
unaryType Tail   = (expectClass Inductive, \(TList t) -> t)


binaryType :: Binary -> (Type -> Type -> Either Error (), Type -> Type -> Type)
binaryType op
  | op `elem` [Add, Sub, Mul]      = (expectBoth Numeric, arithmResult)
  | op `elem` [Div, Pow]           = (expectBoth Numeric, const2 TReal) 
  | op `elem` [Mod, IntDiv]        = (expectBoth Integral, const2 TInt)
  | op `elem` [And, Or, Xor]       = (expectBoth BoolLike, const2 TBool)
  | op `elem` [Eq, NEq]            = (expectSame Equatable, const2 TBool)
  | op `elem` [LEq, GEq, LTn, GTn] = (expectSame Comparable, const2 TBool)
  | op == Concat                   = (expectSame Inductive, const)
  | op == Cons                     = (expectCons, flip const) 
  | op == Get                      = (expectGet, getResult) 
  | op == Apply                    = (expectApply, \(TArrow _ t) _ -> t)
  | op == Compose                  = (expectCompose, \(TArrow t t') (TArrow t1 t1') -> TArrow t1' t)


hasClass :: TypeClass -> Type -> Bool 
hasClass tc = (tc `elem`) . typeClass 


expectClass :: TypeClass -> Type -> Either Error ()
expectClass tc t = if tc `elem` typeClass t then Right () else Left $ TypeError ""


expectType :: Type -> Type -> Either Error ()
expectType t t' 
  | t == t'   = Right ()
  | otherwise = Left $ TypeError "" 


expectBoth :: TypeClass -> Type -> Type -> Either Error ()
expectBoth = liftA2 expectClass2 id id 


expectClass2 :: TypeClass -> TypeClass -> Type -> Type -> Either Error ()
expectClass2 tc tc' t t' = expectClass tc t *> expectClass tc' t' 


expectSame :: TypeClass -> Type -> Type -> Either Error ()
expectSame tc t t' = expectBoth tc t t' *> expectType t t' 


const2 :: a -> b -> a -> a 
const2 = \x _ _ -> x 


arithmResult :: Type -> Type -> Type 
arithmResult TInt TInt = TInt 
arithmResult _ _ = TReal 


expectCons :: Type -> Type -> Either Error ()
expectCons t (TList t') = expectType t t' 
expectCons _ _          = Left $ TypeError ""


expectGet :: Type -> Type -> Either Error ()
expectGet (TList t) TInt  = Right ()
expectGet (TArray t) TInt = Right ()
expectGet (TDict t _) t'  = expectType t t' 
expectGet _ _             = Left $ TypeError ""


getResult :: Type -> Type -> Type 
getResult (TList t) _  = t 
getResult (TArray t) _ = t
getResult _ _ = undefined 


expectApply :: Type -> Type -> Either Error ()
expectApply (TArrow t _) = expectType t 


expectCompose :: Type -> Type -> Either Error ()
expectCompose (TArrow t t') (TArrow t1 t1') = expectType t1' t 