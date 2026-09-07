module Value where


import Data.Array.ST(STArray)
import Data.Array.ST qualified as STA 
import Data.Array(Array)
import Data.Array qualified as A 
import Data.Map (Map)
import Data.Map qualified as Map 
import Data.Word 
import Data.Char 
import GHC.Float (castDoubleToWord64)
import Expr 
import HM 


type Env s = Map String (Val s)


data Val s
  = VNothing 
  | VBool Bool 
  | VInt Int 
  | VReal Double 
  | VString (Array Int Char)
  | VList [Val s]
  | VArray (STArray s Int (Val s))
  | VTuple (Array Int (Val s))
  | VDict (HashMap s (Val s) (Val s))
  | VArrow String Expr 


defaultHash :: Val s -> Word64 
defaultHash (VBool b)      = if b then 1 else 0 
defaultHash (VInt n)       = mix $ fromIntegral n 
defaultHash (VReal x)      = mix $ castDoubleToWord64 x 
defaultHash (VList l)      = foldl' (\h x -> h * 31 + defaultHash x) 0 l 
defaultHash (VString s)    = hashArr (fromIntegral . ord) s 
defaultHash (VTuple t)     = hashArr defaultHash t 
defaultHash VNothing       = error "VNothing value is unhashable: Cannot hash value representing undeclared data"
defaultHash (VArray _)     = error "VArray value is unhashable: Cannot hash value from an array object, as it has no equality constraint"
defaultHash (VDict _)      = error "VDict value is unhashable: Cannot hash value from a dictionary object, as it has no equality constraint"
defaultHash (VArrow _ _)   = error "VArrow value is unhashable: Cannot hash value from a function object, as it has no equality constraint (Halting Problem :)"


instance Eq (Val s) where
    VNothing == VNothing     = True 
    VBool b == VBool b'      = b == b' 
    VInt n == VInt n'        = n == n' 
    VReal x == VReal x'      = x == x'
    VList l == VList l'      = l == l' 
    VString s == VString s'  = s == s' 
    VTuple t == VTuple t'    = t == t' 
    VArray _ == VArray _     = error "VArray is not equatable"
    VDict _ == VDict _       = error "VDict is not equatable" 
    VArrow _ _ == VArrow _ _ = error "VArrow is not equatable"
