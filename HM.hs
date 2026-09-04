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
import Data.Array.ST qualified as STA 
import Data.STRef
import Expr 
import Eval 
import GHC.Base (RuntimeRep(Int16Rep))
import GHC.Arr (negRange, newSTArray)
import Data.List (foldl')

initialCapacity :: Int 
initialCapacity = 16 

data HashMap s = HashMap
  { buckets  :: STRef s (STArray s Int [(Val s, Val s)])
  , size     :: STRef s Int 
  , capacity :: STRef s Int 
  , hash     :: Val s -> Word64 
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


empty :: Maybe Int -> (Val s -> Word64) -> ST s (HashMap s) 
empty n h = do 
  let cap = maybe initialCapacity id n 
  size     <- newSTRef 0 
  capacity <- newSTRef cap
  arr      <- STA.newArray (0, cap - 1) []
  buckets  <- newSTRef arr 
  pure $ HashMap buckets size capacity h 


index :: HashMap s -> Val s -> ST s Int 
index hm k = do 
  cap  <- readSTRef $ capacity hm
  pure $ (fromIntegral $ abs $ hash hm $ k) `mod` cap  


lookup :: HashMap s -> Val s -> ST s (Maybe (Val s))
lookup hm k = do 
  i      <- index hm k 
  arr    <- readSTRef $ buckets hm 
  bucket <- STA.readArray arr i 
  pure $ Prelude.lookup k bucket 


insert :: HashMap s -> (Val s, Val s) -> ST s ()
insert hm p = do 
  siz <- readSTRef $ size hm
  cap <- readSTRef $ capacity hm
  when (realToFrac siz / realToFrac cap >= 0.75) $ resize hm
  i   <- index hm $ fst p 
  arr <- readSTRef $ buckets hm 
  l   <- STA.readArray arr i 
  let new = p : l 
  STA.writeArray arr i new  
  modifySTRef' (size hm) (+ 1)


resize :: HashMap s -> ST s ()
resize hm = do 
  cap <- readSTRef $ capacity hm 
  let newCap = 2 * cap 
  old <- readSTRef $ buckets hm 
  l   <- STA.getElems $ old  
  new <- STA.newArray (0, newCap - 1) [] :: ST s (STArray s Int [(Val s, Val s)])
  writeSTRef (buckets hm) new 
  writeSTRef (capacity hm) newCap 
  forM_ (concat l) $ insert hm


fromList :: (Val s -> Word64) -> [(Val s, Val s)] -> ST s (HashMap s)
fromList f l = do 
  hm       <- empty Nothing f
  forM_ l (insert hm)
  pure hm 