module TypeSystem where 


import Type 
import Expr 
import Error 
import Control.Applicative 


import Data.Map(Map)
import Data.Map qualified as Map 


type Context = Map String Type 


data TypeClass 
    = Equatable 
    | Comparable 
    | Numeric 
    | Integral
    | BoolLike 
    | Collectable 
    | Inductive 
    deriving Eq 


typecheck :: Context -> Expr -> Either TypeError Type 
typecheck c (Lit l)               = typecheckLit c l 
typecheck _ (Def d)               = typecheckDef d 
typecheck c (Var s)               = case Map.lookup s c of 
    Just t                       -> Right t 
    _                            -> Left $ TypeError ""
typecheck c (Unary op e)          = typecheckUnary c op e 
typecheck c (Binary op e e')      = typecheckBinary c op e e' 
typecheck c (Ternary op1 e e1 e2) = typecheckTernary c op1 e e1 e2 
typecheck c (Let s e e')          = typecheckLet c s e e' 
typecheck c (If e e' e'')         = typecheckIf c e e' e'' 


typecheckLit :: Context -> Lit -> Either TypeError Type 
typecheckLit _ LTrue             = Right TBool
typecheckLit _ LFalse            = Right TBool 
typecheckLit _ (LInt _)          = Right TInt 
typecheckLit _ (LReal _)         = Right TReal 
typecheckLit _ (LString _)       = Right TString 
typecheckLit c (LList l)         = TList <$> typecheckList c l 
typecheckLit c (LListEmpty e)    = TList <$> typecheck c e 
typecheckLit c (LArray a)        = TArray <$> typecheckList c a 
typecheckLit c (LArrayEmpty e)   = TArray <$> typecheck c e 
typecheckLit c (LTuple t)        = TTuple <$> traverse (typecheck c) t  
typecheckLit c (LDict d)         = uncurry TDict <$> typecheckDict c d
typecheckLit c (LDictEmpty e e') = TDict <$> typecheck c e <*> typecheck c e'  
typecheckLit c (LArrow s te e)   = uncurry TArrow <$> typecheckArrow c s te e 



typecheckList :: Context -> [Expr] -> Either TypeError Type 
typecheckList c (e:es) = do 
    te <- typecheck c e 
    tes <- traverse (typecheck c) es 
    if all (== te) tes then Right te else Left $ TypeError ""
typecheckList _ _ = undefined  
 

typecheckDict :: Context -> [(Expr, Expr)] -> Either TypeError (Type, Type)
typecheckDict c ((e,e'):es) = do 
    (te, te') <- (,) <$> typecheck c e <*> typecheck c e' 
    let (es1, es2) = unzip es
    (tes, tes') <- (,) <$> traverse (typecheck c) es1 <*> traverse (typecheck c) es2 
    if all (== te) tes && all (== te') tes' then do
        expectClass Equatable te 
        Right (te, te') 
    else Left $ TypeError ""  
typecheckDict _ _ = undefined 


typecheckArrow :: Context -> String -> Expr -> Expr -> Either TypeError (Type, Type)
typecheckArrow c s e e' = do 
    te <- typecheck c e 
    let new = Map.insert s te c 
    (te, ) <$> typecheck new e' 


typecheckDef :: Def -> Either TypeError Type 
typecheckDef DBool         = Right TBool 
typecheckDef DInt          = Right TInt 
typecheckDef DReal         = Right TReal 
typecheckDef DString       = Right TString 
typecheckDef (DList t)     = TList <$> typecheckDef t
typecheckDef (DArray t)    = TArray <$> typecheckDef t 
typecheckDef (DTuple ts)   = TTuple <$> traverse (typecheckDef) ts 
typecheckDef (DDict t t')  = do 
    tk <- typecheckDef t 
    expectClass Equatable tk 
    TDict tk <$> typecheckDef t' 
typecheckDef (DArrow t t') = TArrow <$> typecheckDef t <*> typecheckDef t' 


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


typecheckUnary :: Context -> Unary -> Expr -> Either TypeError Type
typecheckUnary c op e = do 
    te <- typecheck c e 
    let (check, result) = unaryType op 
    check te 
    Right $ result te


unaryType :: Unary -> (Type -> Either TypeError (), Type -> Type)
unaryType Not    = (expectClass BoolLike, const TBool)
unaryType Neg    = (expectClass Numeric, id)
unaryType ToInt  = (expectClass Numeric, const TInt)
unaryType ToReal = (expectClass Numeric, const TReal) 
unaryType Fact   = (expectClass Integral, const TInt)
unaryType Len    = (expectClass Collectable, const TInt)
unaryType Head   = (expectClass Inductive, \(TList t) -> t)
unaryType Tail   = (expectClass Inductive, \(TList t) -> t)


typecheckBinary :: Context -> Binary -> Expr -> Expr -> Either TypeError Type 
typecheckBinary c Project e i = do 
    te <- typecheck c e 
    ti <- typecheck c i 
    expectType ti TInt 
    case (te, i) of 
        (TTuple ts, Lit (LInt n)) 
          | n >= 0 && n < length ts -> Right (ts !! n)
          | otherwise               -> Left $ TypeError ""
        _                           -> Left $ TypeError ""
typecheckBinary c op e e' = do 
    te  <- typecheck c e 
    te' <- typecheck c e'
    let (check, result) = binaryType op 
    check te te' 
    Right $ result te te'  


binaryType :: Binary -> (Type -> Type -> Either TypeError (), Type -> Type -> Type)
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


typecheckTernary :: Context -> Ternary -> Expr -> Expr -> Expr -> Either TypeError Type 
typecheckTernary c op e1 e2 e3 = do 
    (t1, t2, t3) <- liftA3 (,,) (typecheck c e1) (typecheck c e2) (typecheck c e3)
    case (op, t1, t2, t3) of 
        (Set, TArray t, TInt, t') -> do 
            expectType t t' 
            Right $ TArray t 
        (Set, TDict t t', k, v)   -> do 
            expectType t k 
            expectType t' v 
            Right $ TDict t t' 
        _                         -> Left $ TypeError ""


typecheckLet :: Context -> String -> Expr -> Expr -> Either TypeError Type 
typecheckLet c s e e' = do 
    te <- typecheck c e 
    let new = Map.insert s te c 
    typecheck new e' 


typecheckIf :: Context -> Expr -> Expr -> Expr -> Either TypeError Type 
typecheckIf c e e' e'' = do  
    (te, te', te'') <- liftA3 (,,) (typecheck c e) (typecheck c e') (typecheck c e'')
    expectType te TBool 
    expectType te' te'' 
    Right te' 