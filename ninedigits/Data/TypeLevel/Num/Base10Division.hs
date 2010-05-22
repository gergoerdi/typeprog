{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances,
             TypeOperators,
             TemplateHaskell #-}
module Data.TypeLevel.Num.Base10Division (Divides) where

import Data.TypeLevel.Num.Base10Division.TH    
import Data.TypeLevel.Bool
import Data.TypeLevel.Num
import Data.TypeLevel.Num.Reps
import Data.TypeLevel.Num.Ops (Add)
import Data.TypeLevel.List
    
$(divBase 10)
 
instance (Divides q ds b) => Divides q (D0 ::: ds) b
instance (Divides q (d'':::ds) b, Sub (d :* d') q d'' ) => Divides q ((d :* d') ::: ds) b       
