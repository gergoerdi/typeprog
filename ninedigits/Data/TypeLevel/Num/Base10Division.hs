{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances,
             NoImplicitPrelude,
             TypeOperators,
             TemplateHaskell #-}
module Data.TypeLevel.Num.Base10Division (Divides) where

import Data.TypeLevel.Num.Base10Division.TH    
import Data.TypeLevel.Bool
import Data.TypeLevel.Num
import Data.TypeLevel.Num.Reps
import Prelude (undefined, fromInteger)
import Data.TypeLevel.List
    
-- class Divides q p b | q p -> b where
--     div :: q -> p -> b
--     div = undefined
            
instance Divides q Nil True

-- $(resultsFalse 9)
instance Divides D2 (D1 :::Nil) False
    
instance Divides D3 (D1 :::Nil) False
instance Divides D3 (D2 :::Nil) False

instance Divides D4 (D1 :::Nil) False
instance Divides D4 (D2 :::Nil) False
instance Divides D4 (D3 :::Nil) False

instance Divides D5 (D1 :::Nil) False
instance Divides D5 (D2 :::Nil) False
instance Divides D5 (D3 :::Nil) False
instance Divides D5 (D4 :::Nil) False

instance Divides D6 (D1 :::Nil) False
instance Divides D6 (D2 :::Nil) False
instance Divides D6 (D3 :::Nil) False
instance Divides D6 (D4 :::Nil) False
instance Divides D6 (D5 :::Nil) False

instance Divides D7 (D1 :::Nil) False
instance Divides D7 (D2 :::Nil) False
instance Divides D7 (D3 :::Nil) False
instance Divides D7 (D4 :::Nil) False
instance Divides D7 (D5 :::Nil) False
instance Divides D7 (D6 :::Nil) False

instance Divides D8 (D1 :::Nil) False
instance Divides D8 (D2 :::Nil) False
instance Divides D8 (D3 :::Nil) False
instance Divides D8 (D4 :::Nil) False
instance Divides D8 (D5 :::Nil) False
instance Divides D8 (D6 :::Nil) False
instance Divides D8 (D7 :::Nil) False

instance Divides D9 (D1 :::Nil) False
instance Divides D9 (D2 :::Nil) False
instance Divides D9 (D3 :::Nil) False
instance Divides D9 (D4 :::Nil) False
instance Divides D9 (D5 :::Nil) False
instance Divides D9 (D6 :::Nil) False
instance Divides D9 (D7 :::Nil) False
instance Divides D9 (D8 :::Nil) False
    
--instance (Divides q ds b) => Divides q (q ::: ds) b
instance (Divides q ds b) => Divides q (D0 ::: ds) b

-- Division by 1
instance (Divides D1 (D0 ::: ds) b) => Divides D1 (D1 ::: ds) b
instance (Divides D1 (D1 ::: ds) b) => Divides D1 (D2 ::: ds) b
instance (Divides D1 (D2 ::: ds) b) => Divides D1 (D3 ::: ds) b
instance (Divides D1 (D3 ::: ds) b) => Divides D1 (D4 ::: ds) b
instance (Divides D1 (D4 ::: ds) b) => Divides D1 (D5 ::: ds) b
instance (Divides D1 (D5 ::: ds) b) => Divides D1 (D6 ::: ds) b
instance (Divides D1 (D6 ::: ds) b) => Divides D1 (D7 ::: ds) b
instance (Divides D1 (D7 ::: ds) b) => Divides D1 (D8 ::: ds) b
instance (Divides D1 (D8 ::: ds) b) => Divides D1 (D9 ::: ds) b
    
-- Division by 2
instance (Divides D2 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D2 (D1 ::: d ::: ds) b
instance (Divides D2 (D0 ::: ds) b) => Divides D2 (D2 ::: ds) b
instance (Divides D2 (D1 ::: ds) b) => Divides D2 (D3 ::: ds) b
instance (Divides D2 (D2 ::: ds) b) => Divides D2 (D4 ::: ds) b
instance (Divides D2 (D3 ::: ds) b) => Divides D2 (D5 ::: ds) b
instance (Divides D2 (D4 ::: ds) b) => Divides D2 (D6 ::: ds) b
instance (Divides D2 (D5 ::: ds) b) => Divides D2 (D7 ::: ds) b
instance (Divides D2 (D6 ::: ds) b) => Divides D2 (D8 ::: ds) b
instance (Divides D2 (D7 ::: ds) b) => Divides D2 (D9 ::: ds) b
    
-- Division by 3
instance (Divides D3 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D3 (D1 ::: d ::: ds) b
instance (Divides D3 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D3 (D2 ::: d ::: ds) b    
instance (Divides D3 (D0 ::: ds) b) => Divides D3 (D3 ::: ds) b
instance (Divides D3 (D1 ::: ds) b) => Divides D3 (D4 ::: ds) b
instance (Divides D3 (D2 ::: ds) b) => Divides D3 (D5 ::: ds) b
instance (Divides D3 (D3 ::: ds) b) => Divides D3 (D6 ::: ds) b
instance (Divides D3 (D4 ::: ds) b) => Divides D3 (D7 ::: ds) b
instance (Divides D3 (D5 ::: ds) b) => Divides D3 (D8 ::: ds) b
instance (Divides D3 (D6 ::: ds) b) => Divides D3 (D9 ::: ds) b

-- Division by 4
instance (Divides D4 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D4 (D1 ::: d ::: ds) b
instance (Divides D4 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D4 (D2 ::: d ::: ds) b    
instance (Divides D4 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D4 (D3 ::: d ::: ds) b    
instance (Divides D4 (D0 ::: ds) b) => Divides D4 (D4 ::: ds) b
instance (Divides D4 (D1 ::: ds) b) => Divides D4 (D5 ::: ds) b
instance (Divides D4 (D2 ::: ds) b) => Divides D4 (D6 ::: ds) b
instance (Divides D4 (D3 ::: ds) b) => Divides D4 (D7 ::: ds) b
instance (Divides D4 (D4 ::: ds) b) => Divides D4 (D8 ::: ds) b
instance (Divides D4 (D5 ::: ds) b) => Divides D4 (D9 ::: ds) b

-- Division by 5
instance (Divides D5 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D5 (D1 ::: d ::: ds) b
instance (Divides D5 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D5 (D2 ::: d ::: ds) b    
instance (Divides D5 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D5 (D3 ::: d ::: ds) b    
instance (Divides D5 (d' ::: ds) b, Add (D4 :* D0) d d') => Divides D5 (D4 ::: d ::: ds) b    
instance (Divides D5 (D0 ::: ds) b) => Divides D5 (D5 ::: ds) b
instance (Divides D5 (D1 ::: ds) b) => Divides D5 (D6 ::: ds) b
instance (Divides D5 (D2 ::: ds) b) => Divides D5 (D7 ::: ds) b
instance (Divides D5 (D3 ::: ds) b) => Divides D5 (D8 ::: ds) b
instance (Divides D5 (D4 ::: ds) b) => Divides D5 (D9 ::: ds) b

-- Division by 6
instance (Divides D6 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D6 (D1 ::: d ::: ds) b
instance (Divides D6 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D6 (D2 ::: d ::: ds) b    
instance (Divides D6 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D6 (D3 ::: d ::: ds) b    
instance (Divides D6 (d' ::: ds) b, Add (D4 :* D0) d d') => Divides D6 (D4 ::: d ::: ds) b    
instance (Divides D6 (d' ::: ds) b, Add (D5 :* D0) d d') => Divides D6 (D5 ::: d ::: ds) b    
instance (Divides D6 (D0 ::: ds) b) => Divides D6 (D6 ::: ds) b
instance (Divides D6 (D1 ::: ds) b) => Divides D6 (D7 ::: ds) b
instance (Divides D6 (D2 ::: ds) b) => Divides D6 (D8 ::: ds) b
instance (Divides D6 (D3 ::: ds) b) => Divides D6 (D9 ::: ds) b

-- Division by 7
instance (Divides D7 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D7 (D1 ::: d ::: ds) b
instance (Divides D7 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D7 (D2 ::: d ::: ds) b    
instance (Divides D7 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D7 (D3 ::: d ::: ds) b    
instance (Divides D7 (d' ::: ds) b, Add (D4 :* D0) d d') => Divides D7 (D4 ::: d ::: ds) b    
instance (Divides D7 (d' ::: ds) b, Add (D5 :* D0) d d') => Divides D7 (D5 ::: d ::: ds) b    
instance (Divides D7 (d' ::: ds) b, Add (D6 :* D0) d d') => Divides D7 (D6 ::: d ::: ds) b    
instance (Divides D7 (D0 ::: ds) b) => Divides D7 (D7 ::: ds) b
instance (Divides D7 (D1 ::: ds) b) => Divides D7 (D8 ::: ds) b
instance (Divides D7 (D2 ::: ds) b) => Divides D7 (D9 ::: ds) b

-- Division by 8
instance (Divides D8 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D8 (D1 ::: d ::: ds) b
instance (Divides D8 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D8 (D2 ::: d ::: ds) b    
instance (Divides D8 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D8 (D3 ::: d ::: ds) b    
instance (Divides D8 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D8 (D4 ::: d ::: ds) b    
instance (Divides D8 (d' ::: ds) b, Add (D4 :* D0) d d') => Divides D8 (D5 ::: d ::: ds) b    
instance (Divides D8 (d' ::: ds) b, Add (D5 :* D0) d d') => Divides D8 (D6 ::: d ::: ds) b    
instance (Divides D8 (d' ::: ds) b, Add (D6 :* D0) d d') => Divides D8 (D7 ::: d ::: ds) b    
instance (Divides D8 (D0 ::: ds) b) => Divides D8 (D8 ::: ds) b
instance (Divides D8 (D1 ::: ds) b) => Divides D8 (D9 ::: ds) b

    
-- Division by 9    
instance (Divides D9 (d' ::: ds) b, Add (D1 :* D0) d d') => Divides D9 (D1 ::: d ::: ds) b
instance (Divides D9 (d' ::: ds) b, Add (D2 :* D0) d d') => Divides D9 (D2 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D3 :* D0) d d') => Divides D9 (D3 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D4 :* D0) d d') => Divides D9 (D4 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D5 :* D0) d d') => Divides D9 (D5 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D6 :* D0) d d') => Divides D9 (D6 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D7 :* D0) d d') => Divides D9 (D7 ::: d ::: ds) b    
instance (Divides D9 (d' ::: ds) b, Add (D8 :* D0) d d') => Divides D9 (D8 ::: d ::: ds) b    
instance (Divides D9 (D0 ::: ds) b) => Divides D9 (D9 ::: ds) b

    
instance (Divides q (d'':::ds) b, Sub (d :* d') q d'' ) => Divides q ((d :* d') ::: ds) b       
