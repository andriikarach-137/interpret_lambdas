module Error where 


import Control.Applicative


data Error
    = ParseError String 
    | TypeError String
    | EvalError String 
    | EmptyError 

instance Alternative (Either Error) where
    empty = Left EmptyError  

    te <|> te' = case te of 
        Left _ -> te' 
        _      -> te 
