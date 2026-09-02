module HM where 


import Data.Array.ST(STArray)
import Data.Array.ST qualified as A 
import Data.STRef (STRef)
import Expr 


data HashMap s k v = HashMap
  { buckets  :: STArray s Int [(k, v)]
  , size     :: STRef s Int 
  , capacity :: STRef s Int 
  , hash     :: k -> Int 
  }