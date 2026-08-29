module Type where 


data Type 
    = TNum TNum 
    | TBool 
    | Ttring 
    | TList Type 
    | TArray Type 
    | TTuple [Type]
    | TDict Type Type 
    | TArrow Type Type 


data TNum 
    = TInt 
    | TReal 