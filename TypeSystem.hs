module TypeSystem where 


import Type 
import TypeUtils 
import Expr 
import Error 
import Control.Applicative 


import Data.Map(Map)
import Data.Map qualified as Map 


type Context = Map String Type 


typecheck :: Context -> Expr -> Either Error Type 
typecheck c (Lit l)               = typecheckLit c l 
typecheck _ (Def d)               = typecheckDef d 
typecheck c (Var s)               = case Map.lookup s c of 
    Just t                       -> Right t 
    _                            -> Left $ TypeError ""
typecheck c (Unary op e)          = typecheckUnary c op e 
typecheck c (Binary op e e')      = typecheckBinary c op e e' 
typecheck c (Let s e e')          = typecheckLet c s e e' 
typecheck c (If e e' e'')         = typecheckIf c e e' e'' 


typecheckLit :: Context -> Lit -> Either Error Type 
typecheckLit _ LTrue             = Right TBool
typecheckLit _ LFalse            = Right TBool 
typecheckLit _ (LInt _)          = Right TInt 
typecheckLit _ (LReal _)         = Right TReal 
typecheckLit _ (LString _)       = Right TString 
typecheckLit c (LList l)         = TList <$> typecheckList c l 
typecheckLit c (LListEmpty e)    = TList <$> typecheck c e 
typecheckLit c (LArray a _)      = TArray <$> typecheckList c a 
typecheckLit c (LTuple t)        = TTuple <$> traverse (typecheck c) t  
typecheckLit c (LDict d)         = uncurry TDict <$> typecheckDict c d
typecheckLit c (LDictEmpty e e') = TDict <$> typecheck c e <*> typecheck c e'  
typecheckLit c (LArrow s te e)   = uncurry TArrow <$> typecheckArrow c s te e 



typecheckList :: Context -> [Expr] -> Either Error Type 
typecheckList c (e:es) = do 
    te <- typecheck c e 
    tes <- traverse (typecheck c) es 
    if all (== te) tes then Right te else Left $ TypeError ""
typecheckList _ _ = undefined  
 

typecheckDict :: Context -> [(Expr, Expr)] -> Either Error (Type, Type)
typecheckDict c ((e,e'):es) = do 
    (te, te') <- (,) <$> typecheck c e <*> typecheck c e' 
    let (es1, es2) = unzip es
    (tes, tes') <- (,) <$> traverse (typecheck c) es1 <*> traverse (typecheck c) es2 
    if all (== te) tes && all (== te') tes' then do
        expectClass Equatable te 
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