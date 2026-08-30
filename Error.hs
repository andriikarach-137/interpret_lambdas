module Error where 


import Control.Applicative


data TypeError 
    = TypeError String | EmptyTypeError 


instance Alternative (Either TypeError) where
    empty = Left EmptyTypeError 

    te <|> te' = case te of 
        Left _ -> te' 
        _      -> te 
