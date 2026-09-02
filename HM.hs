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


data HashMap s = HashMap
  { buckets  :: STArray s Int [(Val s, Val s)]
  , size     :: STRef s Int 
  , capacity :: STRef s Int 
  , hash     :: Val s -> Int 
  }


defaultHash :: Val s -> Word64 
defaultHash VNothing    = 0 
defaultHash (VBool b)   = if b == True then 1 else 0 
defaultHash (VInt n)    = mix $ fromIntegral n 
defaultHash (VReal x)   = mix $ castDoubleToWord64 x 
defaultHash (VString s) = hashVString s 


hashVString :: Array Int Char -> Word64 
hashVString arr = let (hi, low) = A.bounds arr in foldl step 0 [hi..low]
  where
    step :: Word64 -> Int -> Word64 
    step i n = i * 31 + fromIntegral (ord (arr A.! n)) 


mix :: Word64 -> Word64
mix w = 
  let w1 = w `xor` (w `shiftR` 31)
      w2 = w1 * 0xb13579BDF02468AC 
      w3 = w2 `xor` (w2 `shiftR` 33)
      w4 = w3 * 0xb21347861234ABD7 
  in w4 `xor` (w4 `shiftR` 29) 