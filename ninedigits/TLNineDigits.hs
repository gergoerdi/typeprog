{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances,
             NoImplicitPrelude,
             TypeOperators #-}
module Main where

import Data.TypeLevel.Bool
import Data.TypeLevel.Num
import Prelude (undefined, print)

import Data.TypeLevel.List
import Data.TypeLevel.Num.Base10Division
    
class IsEq c b | c -> b
instance IsEq EQ True
instance IsEq LT False
instance IsEq GT False

-- IntEq :: Int -> Int -> Bool    
class IntEq x y b | x y -> b    
instance (Trich x y c, IsEq c b) => IntEq x y b

-- Contains :: Int -> [Int] -> Int
class Contains x ys b | x ys -> b
instance Contains x Nil False
instance (IntEq x y b, Contains x ys b', Or b b' b'') => Contains x (y ::: ys) b''

-- Value :: [Int] -> Int    
class Value ds n | ds -> n
instance (Reverse ds ds', Value' ds' n) => Value ds n
    
class Value' ds n | ds -> n
instance Value' (d ::: Nil) d
instance (Value' (d' ::: ds) n) => Value' (d ::: d' ::: ds) (d :* n)
    
-- DivisorTest :: [Int] -> Bool    
class DivisorTest ds b | ds -> b where
instance (Reverse ds ds', Length ds l, Divides l ds' b) => DivisorTest ds b

-- TestCandidate :: [Int] -> Bool    
class TestCandidate ds b | ds -> b where
instance (Contains d ds b, Not b b', DivisorTest (d ::: ds) b'', And b' b'' b''') => TestCandidate (d ::: ds) b'''

-- SearchI :: Int -> Bool -> Bool -> [Int] -> Int    
class SearchI len good final ds v | len good final ds -> v where
instance (Value' ds v) => SearchI len True True ds v
instance (Search len (D1 ::: ds) v) => SearchI len True False ds v
instance (Succ d d', Search len (d' ::: ds) v) => SearchI len False False (d ::: ds) v
    
-- Search :: Int -> [Int] -> Int
class Search len ds v | len ds -> v where
instance (Search len (D1 ::: Nil) v) => Search len Nil v
instance Search len (D10 ::: Nil) False
instance (Succ d d', Search len (d' ::: ds) v) => Search len (D10 ::: d ::: ds) v

-- instance (TestCandidate (d ::: ds) good, Length (d ::: ds) l, IntEq len l b, And good b final, SearchI len good final (d ::: ds) v) => Search len (d ::: ds) v
instance (TestCandidate (D1 ::: ds) good, Length (D1 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D1 ::: ds) v) => Search len (D1 ::: ds) v
instance (TestCandidate (D2 ::: ds) good, Length (D2 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D2 ::: ds) v) => Search len (D2 ::: ds) v
instance (TestCandidate (D3 ::: ds) good, Length (D3 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D3 ::: ds) v) => Search len (D3 ::: ds) v
instance (TestCandidate (D4 ::: ds) good, Length (D4 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D4 ::: ds) v) => Search len (D4 ::: ds) v
instance (TestCandidate (D5 ::: ds) good, Length (D5 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D5 ::: ds) v) => Search len (D5 ::: ds) v
instance (TestCandidate (D6 ::: ds) good, Length (D6 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D6 ::: ds) v) => Search len (D6 ::: ds) v
instance (TestCandidate (D7 ::: ds) good, Length (D7 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D7 ::: ds) v) => Search len (D7 ::: ds) v
instance (TestCandidate (D8 ::: ds) good, Length (D8 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D8 ::: ds) v) => Search len (D8 ::: ds) v
instance (TestCandidate (D9 ::: ds) good, Length (D9 ::: ds) l, IntEq len l b, And good b final, SearchI len good final (D9 ::: ds) v) => Search len (D9 ::: ds) v


search :: Search len ds v => len -> ds -> v    
search = undefined

--test = search (undefined :: D7) (undefined :: Nil)
test = search (undefined :: D6) (undefined :: D8 ::: D3 ::: Nil)
--test = search (undefined :: D7) (undefined :: D9 ::: D2 ::: D1 ::: Nil)
--test' = search (undefined :: D1) (undefined :: D10 ::: Nil)
--test = search (undefined :: D3) (undefined :: Nil)

-- 381654729       
sol = undefined :: D3 :* D8 :* D1 :* D6 :* D5 :* D4 :* D7 :* D2 :* D9
sol' = undefined :: D9 ::: D2 ::: D7 ::: D4 ::: D5 ::: D6 ::: D1 ::: D8 ::: D3 ::: Nil
sol'' = undefined :: D3 ::: D8 ::: D1 ::: D6 ::: D5 ::: D4 ::: D7 ::: D2 ::: D9 ::: Nil

main = print test       
