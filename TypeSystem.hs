module TypeSystem where 


import Type 
import TypeUtils 
import Expr 
import Error 
import Control.Applicative 


import Data.Map(Map)
import Data.Map qualified as Map 


type Context = Map String Type 


typeOf :: TypedExpr -> Type 
typeOf (TLit t _)        = t 
typeOf (TDef t _)        = t 
typeOf (TVar t _)        = t 
typeOf (TUnary t _ _)    = t 
typeOf (TBinary t _ _ _) = t
typeOf (TLet t _ _ _)    = t 
typeOf (TIf t _ _ _)     = t 


data TypedExpr
    = TLit Type Lit 
    | TCList Type [TypedExpr]
    | TCArray Type [TypedExpr] Int 
    | TCTuple Type [TypedExpr]
    | TCDict Type [(TypedExpr, TypedExpr)] 
    | TDef Type Def 
    | TVar Type String 
    | TUnary Type Unary Expr 
    | TBinary Type Binary Expr Expr 
    | TLet Type String Expr Expr 
    | TIf Type Expr Expr Expr 
    | TArrow Type String Type TypedExpr 
    deriving Eq


typecheck :: Context -> Expr -> Either Error TypedExpr 
typecheck _ (Lit l)  = typecheckLit l 
typecheck c (Col xs) = typecheckCol c xs  


typecheckLit :: Lit -> Either Error TypedExpr 
typecheckLit x@LTrue       = Right $ TLit TBool x  
typecheckLit x@LFalse      = Right $ TLit TBool x 
typecheckLit x@(LInt _)    = Right $ TLit TInt x 
typecheckLit x@(LReal _)   = Right $ TLit TReal x 
typecheckLit x@(LString _) = Right $ TLit TString x 


typecheckCol :: Context -> Col -> Either Error TypedExpr 
typecheckCol c (CList l)         = uncurry TCList <$> typecheckElems TList c l
typecheckCol c (CListEmpty e)    = TCList <$> (TList . typeOf <$> typecheck c e) <*> pure []
typecheckCol c (CArray l i)      = uncurry TCArray <$> typecheckElems TArray c l <*> pure i 
typecheckCol c (CTuple l)        = uncurry TCTuple <$> typecheckTuple c l
typecheckCol c (CDict l)         = uncurry TCDict <$> typecheckDict c l 
typecheckCol c (CDictEmpty e e') = TCDict <$> (TDict <$> (typeOf <$> typecheck c e) <*> (typeOf <$> typecheck c e')) <*> pure []


typecheckElems :: (Type -> Type) -> Context -> [Expr] -> Either Error (Type, [TypedExpr]) 
typecheckElems f c (x:xs) = do 
    tex  <- typecheck c x 
    texs <- traverse (typecheck c) xs 
    let (tx, txs) = (typeOf tex, typeOf <$> texs)
    if all (== tx) txs then Right $ (f tx, tex:texs) else Left $ TypeError ""


typecheckTuple :: Context -> [Expr] -> Either Error (Type, [TypedExpr])
typecheckTuple c xs = (\ts -> (TTuple $ typeOf <$> ts, ts)) <$> traverse (typecheck c) xs 


typecheckDict :: Context -> [(Expr, Expr)] -> Either Error (Type, [(TypedExpr, TypedExpr)])
typecheckDict c ((k, v):xs) = do 
    (tek, tev) <- (,) <$> typecheck c k <*> typecheck c v  
    let (tk, tv) = (typeOf tek, typeOf tev)
    let (kxs, vxs) = unzip xs 
    (tekxs, tevxs) <- (,) <$> traverse (typecheck c) kxs <*> traverse (typecheck c) vxs  
    let (tkxs, tvxs) = (typeOf <$> tekxs, typeOf <$> tevxs)
    if all (== tk) tkxs && all (== tv) tvxs then do 
        expectClass Equatable $ tk 
        Right (TDict tk tv, zip (tek:tekxs) (tev:tevxs))
    else Left $ TypeError ""


{-
typecheckDict :: Context -> [(Expr, Expr)] -> Either Error (Type, Type)
typecheckDict c ((e,e'):es) = do 
    (te, te') <- (,) <$> typecheck c e <*> typecheck c e' 
    let (es1, es2) = unzip es
    (tes, tes') <- (,) <$> traverse (typecheck c) es1 <*> traverse (typecheck c) es2 
    if all (== te) tes && all (== te') tes' then do
        expectClass Equatable $ type_ te 
        Right (te, te') 
    else Left $ TypeError ""  
typecheckDict _ _ = undefined 


typecheckArrow :: Context -> String -> Expr -> Expr -> Either Error (Type, Type)
typecheckArrow c s e e' = do 
    te <- typecheck c e 
    let new = Map.insert s te c 
    (te, ) <$> typecheck new e' 


typecheckDef :: Def -> Either Error Type 
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


typecheckUnary :: Context -> Unary -> Expr -> Either Error Type
typecheckUnary c op e = do 
    te <- typecheck c e 
    let (check, result) = unaryType op 
    check te 
    Right $ result te


typecheckBinary :: Context -> Binary -> Expr -> Expr -> Either Error Type 
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


typecheckLet :: Context -> String -> Expr -> Expr -> Either Error Type 
typecheckLet c s e e' = do 
    te <- typecheck c e 
    let new = Map.insert s te c 
    typecheck new e' 


typecheckIf :: Context -> Expr -> Expr -> Expr -> Either Error Type 
typecheckIf c e e' e'' = do  
    (te, te', te'') <- liftA3 (,,) (typecheck c e) (typecheck c e') (typecheck c e'')
    expectType te TBool 
    expectType te' te'' 
    Right te'
-}