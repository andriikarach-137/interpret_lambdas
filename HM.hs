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
import GHC.Base (RuntimeRep(Int16Rep))
import GHC.Arr (negRange, newSTArray)
import Data.List (foldl')
import Data.Monoid(Any(..))


initialCapacity :: Int 
initialCapacity = 16 

data HashMap s k v = HashMap
  { buckets  :: STRef s (STArray s Int [(k, v)])
  , size     :: STRef s Int 
  , capacity :: STRef s Int 
  , hash     :: k -> Word64 
  }


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


empty :: Maybe Int -> (k -> Word64) -> ST s (HashMap s k v) 
empty n h = do 
  let cap = maybe initialCapacity id n 
  size     <- newSTRef 0 
  capacity <- newSTRef cap
  arr      <- STA.newArray (0, cap - 1) []
  buckets  <- newSTRef arr 
  pure $ HashMap buckets size capacity h 


index :: HashMap s k v -> k -> ST s Int 
index hm k = do 
  cap  <- readSTRef $ capacity hm
  pure $ (fromIntegral $ abs $ hash hm $ k) `mod` cap  


lookup :: Eq k => HashMap s k v -> k -> ST s (Maybe v)
lookup hm k = do 
  i      <- index hm k 
  arr    <- readSTRef $ buckets hm 
  bucket <- STA.readArray arr i 
  pure $ Prelude.lookup k bucket 


insertOrReplace :: Eq k => (k, v) -> [(k, v)] -> (Any, [(k, v)]) 
insertOrReplace p []          = (Any False, [p])
insertOrReplace p@(k, _) (p'@(k', _):xs)
  | k == k'   = (Any True, p:xs)  
  | otherwise = (:) <$> (Any False, p') <*> insertOrReplace p xs 


insert :: Eq k => HashMap s k v -> (k, v) -> ST s ()
insert hm p = do 
  cap <- readSTRef $ capacity hm
  i   <- index hm $ fst p 
  arr <- readSTRef $ buckets hm 
  l   <- STA.readArray arr i 
  let (found, new) = insertOrReplace p l 
  STA.writeArray arr i new  
  when (not . getAny $ found) $ modifySTRef' (size hm) (+ 1)
  siz <- readSTRef $ size hm
  when (realToFrac siz / realToFrac cap >= 0.75) $ resize hm


resize :: Eq k => HashMap s k v -> ST s ()
resize hm = do 
  cap <- readSTRef $ capacity hm 
  let newCap = 2 * cap 
  old <- readSTRef $ buckets hm 
  l   <- STA.getElems $ old  
  new <- STA.newArray (0, newCap - 1) [] :: ST s (STArray s Int [(k, v)])
  writeSTRef (buckets hm) new 
  writeSTRef (capacity hm) newCap 
  forM_ (concat l) $ insert hm


fromList :: Eq k => (k -> Word64) -> [(k, v)] -> ST s (HashMap s k v)
fromList f l = do 
  hm  <- empty Nothing f
  forM_ l (insert hm)
  pure hm 