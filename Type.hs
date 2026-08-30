module Type where 


data Type 
    = TInt 
    | TReal 
    | TBool 
    | TString 
    | TList Type 
    | TArray Type 
    | TTuple [Type]
    | TDict Type Type 
    | TArrow Type Type 
    deriving Eq 