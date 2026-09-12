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
typeOf (TCList t _)      = t 
typeOf (TCArray t _ _)   = t 
typeOf (TCTuple t _)     = t
typeOf (TCDict t _)      = t 
typeOf (TDef t _)        = t 
typeOf (TVar t _)        = t 
typeOf (TUnary t _ _)    = t 
typeOf (TBinary t _ _ _) = t
typeOf (TLet t _ _ _)    = t 
typeOf (TIf t _ _ _)     = t 
typeOf (TEArrow t _ _)   = t 


data TypedExpr
    = TLit Type Lit 
    | TCList Type [TypedExpr]
    | TCArray Type [TypedExpr] Int 
    | TCTuple Type [TypedExpr]
    | TCDict Type [(TypedExpr, TypedExpr)] 
    | TDef Type Def 
    | TVar Type String 
    | TUnary Type Unary TypedExpr 
    | TBinary Type Binary TypedExpr TypedExpr 
    | TLet Type String TypedExpr TypedExpr 
    | TIf Type TypedExpr TypedExpr TypedExpr 
    | TEArrow Type String TypedExpr 
    deriving Eq


typecheck :: Context -> Expr -> Either Error TypedExpr 
typecheck _ (Lit l)          = typecheckLit l 
typecheck c (Col xs)         = typecheckCol c xs  
typecheck _ (Def d)          = flip TDef d <$> typecheckDef d 
typecheck c (Var s)          = maybe (Left $ TypeError "") (Right . flip TVar s) (Map.lookup s c) 
typecheck c (Unary op e)     = uncurry (flip TUnary op) <$> typecheckUnary c op e 
typecheck c (Binary op e e') = uncurry3 (flip TBinary op) <$> typecheckBinary c op e e' 
typecheck c (Let s e e')     = uncurry3 (flip TLet s) <$> typecheckLet c s e e' 
typecheck c (If b t e)       = uncurry4 TIf <$> typecheckIf c b t e 
typecheck c (Arrow s e e')   = uncurry (flip TEArrow s) <$> typecheckArrow c s e e' 


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


typecheckUnary :: Context -> Unary -> Expr -> Either Error (Type, TypedExpr)
typecheckUnary c op e = do 
    te   <- typecheck c e 
    let t = typeOf te  
    let (check, result) = unaryType op 
    check t
    Right (result t, te)


typecheckBinary :: Context -> Binary -> Expr -> Expr -> Either Error (Type, TypedExpr, TypedExpr)
typecheckBinary c Project e i = do 
    tee <- typecheck c e 
    tei <- typecheck c i
    let (te, ti) = (typeOf tee, typeOf tei)
    expectType ti TInt 
    case (te, tei) of 
        (TTuple ts, TLit TInt (LInt n)) 
          | n >= 0 && n < length ts -> Right (ts !! n, tee, tei)
          | otherwise               -> Left $ TypeError ""
        _                           -> Left $ TypeError ""
typecheckBinary c op e e' = do 
    tee  <- typecheck c e 
    tee' <- typecheck c e'
    let (te, te') = (typeOf tee, typeOf tee')
    let (check, result) = binaryType op 
    check te te' 
    Right $ (result te te', tee, tee')  


typecheckLet :: Context -> String -> Expr -> Expr -> Either Error (Type, TypedExpr, TypedExpr)
typecheckLet c s e e' = do 
    tee  <- typecheck c e 
    let te = typeOf tee 
    let new = Map.insert s te c 
    tee' <- typecheck new e' 
    Right $ (typeOf tee', tee, tee')


typecheckIf :: Context -> Expr -> Expr -> Expr -> Either Error (Type, TypedExpr, TypedExpr, TypedExpr)
typecheckIf c b t e = do  
    (teb, tet, tee) <- liftA3 (,,) (typecheck c b) (typecheck c t) (typecheck c e)
    let (tb, tt, te) = (typeOf teb, typeOf tet, typeOf tee) 
    expectType tb TBool 
    expectType tt te 
    Right $ (tt, teb, tet, tee)


typecheckArrow :: Context -> String -> Expr -> Expr -> Either Error (Type, TypedExpr)
typecheckArrow c s e e' = do 
    tee  <- typecheck c e 
    let te = typeOf tee 
    let new = Map.insert s te c 
    tee' <- typecheck new e' 
    let te' = typeOf tee' 
    Right $ (TArrow te te', tee') 