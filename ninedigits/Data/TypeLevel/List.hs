{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances,
             NoImplicitPrelude,
             TypeOperators #-}
module Data.TypeLevel.List where

import Data.TypeLevel.Num

data Nil
data x ::: xs
infixr 6 :::

-- Length :: [a] -> Int
class Length xs n | xs -> n
instance Length Nil D0
instance (Length xs n, Succ n n') => Length (x ::: xs) n'
    
-- Reverse :: [a] -> [a]
class Reverse xs ys | xs -> ys
instance Reverse Nil Nil
instance (Reverse xs ys, Append ys (x ::: Nil) ys') => Reverse (x ::: xs) ys'

-- Append :: [a] -> [a] -> [a]
class Append xs ys zs | xs ys -> zs
instance Append Nil ys ys
instance (Append xs ys zs) => Append (x ::: xs) ys (x ::: zs)
