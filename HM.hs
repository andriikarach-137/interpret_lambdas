module HM where 

import Control.Monad.ST 
import Control.Monad 
import Data.Bits 
import Data.Word 
import Data.Char 
import GHC.Float (castDoubleToWord64)
import Data.Array(Array)
import Data.Array qualified as A 
import Data.Array.ST(STArray)
import Data.STRef
import Expr 
import Eval 
import GHC.Base (RuntimeRep(Int16Rep))
import GHC.Arr (negRange)
import Data.List (foldl')


data HashMap s = HashMap
  { buckets  :: STArray s Int [(Val s, Val s)]
  , size     :: STRef s Int 
  , capacity :: STRef s Int 
  , hash     :: Val s -> Int 
  }


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
defaultHash (VArrow _ _ _) = error "VArrow value is unhashable: Cannot hash value from a function object, as it has no equality constraint (Halting Problem :)"
defaultHash _              = error "Value is unhashable, (Left for future development :)"


hashArr :: (a -> Word64) -> Array Int a -> Word64 
hashArr f arr = let (lo, hi) = A.bounds arr in foldl' step 0 [lo..hi]
  where
    step :: Word64 -> Int -> Word64 
    step i n = i * 31 + f (arr A.! n)


mix :: Word64 -> Word64
mix w = 
  let w1 = w `xor` (w `shiftR` 31)
      w2 = w1 * 0xb13579BDF02468AC 
      w3 = w2 `xor` (w2 `shiftR` 33)
      w4 = w3 * 0xb21347861234ABD7 
  in w4 `xor` (w4 `shiftR` 29) 