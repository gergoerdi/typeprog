{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances,
             NoImplicitPrelude,
             TypeOperators #-}
module TLNineDigits where

import Data.TypeLevel.Bool
import Data.TypeLevel.Num
import Prelude (undefined)
    
-- [a] = Nil | a ::: [a]
data Nil
data x ::: xs
infixr 6 :::

-- Length :: [a] -> Int
class Length xs n | xs -> n
instance Length Nil D0
instance (Length xs n, Succ n n') => Length (x ::: xs) n'
    
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
instance Value Nil D0
instance (Value ds n, Mul n D10 n', Add n' d n'') => Value (d ::: ds) n''

-- Divides :: Int -> Int -> Bool    
class Divides q p b | q p -> b where
instance (Mod p q r, IntEq r D0 b) => Divides q p b

-- DivisorTest :: [Int] -> Bool    
class DivisorTest ds b | ds -> b where
instance (Value ds n, Length ds l, Divides l n b) => DivisorTest ds b

-- TestCandidate :: [Int] -> Bool    
class TestCandidate ds b | ds -> b where
instance (Contains d ds b, Not b b', DivisorTest (d ::: ds) b'', And b' b'' b''') => TestCandidate (d ::: ds) b'''

-- SearchI :: Int -> Bool -> Bool -> [Int] -> Int    
class SearchI len good final ds v | len good final ds -> v where
instance (Value ds v) => SearchI len True True ds v
instance (Search len (D1 ::: ds) v) => SearchI len True False ds v
instance (Succ d d', Search len (d' ::: ds) v) => SearchI len False False (d ::: ds) v
    
-- Search :: Int -> [Int] -> Int
class Search len ds v | len ds -> v where
instance (Search len (D1 ::: Nil) v) => Search len Nil v
instance (Sub D0 D1 n1) => Search len (D10 ::: Nil) n1
instance (Succ d d', Search len (d' ::: ds) v) => Search len (D10 ::: d ::: ds) v
instance (TestCandidate (d ::: ds) good, Length (d ::: ds) l, IntEq len l b, And good b final, SearchI len good final (d ::: ds) v) => Search len (d ::: ds) v


search :: Search len ds v => len -> ds -> v    
search = undefined

test = search (undefined :: D9) (undefined :: Nil)
