{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
             #-}
module Data.TypeLevel.Num.Base10Division.TH (Divides, divBase) where

import Data.TypeLevel.List    
import Data.TypeLevel.Bool
    
import Language.Haskell.TH    
import Control.Monad
    
class Divides q p b | q p -> b
instance Divides q Nil True

tdigit d = ConT $ mkName $ "Data.TypeLevel.Num.Reps.D" ++ show d
tnil = ConT $ mkName "Data.TypeLevel.List.Nil"
tfalse = ConT $ mkName "Data.TypeLevel.Bool.False"                  
ttrue = ConT $ mkName "Data.TypeLevel.Bool.True"                  
tvar v = VarT $ mkName v

tcurry = foldl AppT
tdivides q ds b = tcurry (ConT $ mkName "Data.TypeLevel.Num.Base10Division.TH.Divides") [q, ds, b]         
tadd x y z = tcurry (ConT $ mkName "Data.TypeLevel.Num.Ops.Add") [x, y, z]
tsplice d d' = tcurry (ConT $ mkName "Data.TypeLevel.Num.:*") [d, d']
             
x |:| xs = AppT (AppT (ConT $ mkName "Data.TypeLevel.List.:::") x) xs
infixr |:|        
             
finFalseFor q = map finFalse [1..q-1]
    where finFalse p = InstanceD [] (tdivides (tdigit q) (tdigit p |:| tnil) tfalse) []

finFalse k = concatMap finFalseFor [2..k]
                 
decFirstDigits k q = map decFirstDigit [q..k]
    where decFirstDigit d = InstanceD [tdivides (tdigit q) (tdigit (d-q) |:| tvar "ds") (tvar "b")]
                                      (tdivides (tdigit q) (tdigit d |:| tvar "ds") (tvar "b")) []

                                                                                                                 
inheritDec k = concatMap (decFirstDigits k) [1..k]

incDropFirstDigits q = map incDropFirstDigit [1..q-1]
    -- where incDropFirstDigit d = InstanceD [tdivides (tdigit q) (tcons (tvar "d'") (tvar "ds")) (tvar "b"),
    where incDropFirstDigit d = InstanceD [tdivides (tdigit q) (tvar "d'" |:| tvar "ds") (tvar "b"),
                                           tadd (tsplice (tdigit d) (tdigit 0)) (tvar "d") (tvar "d'")]
                                          (tdivides (tdigit q) (tdigit d |:| tvar "d" |:| tvar "ds") (tvar "b")) []
                                                                                                                     
               
inheritDrop k = concatMap incDropFirstDigits [1..k]

divBase k = return $ finFalse (k-1) ++ inheritDec (k-1) ++ inheritDrop (k-1)                
