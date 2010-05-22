{-# LANGUAGE TemplateHaskell,
             EmptyDataDecls,
             MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances,
             TypeOperators #-}
module Data.TypeLevel.Num.Base10Division.TH (Divides, resultsFalse) where

import Language.Haskell.TH    
import Control.Monad
    
class Divides q p b | q p -> b

tdivides = ConT $ mkName "Data.TypeLevel.Num.Base10Division.TH.Divides"
tcurry = foldl AppT
tdigit d = ConT $ mkName $ "Data.TypeLevel.Num.Reps.D" ++ show d
tcons = ConT $ mkName "Data.TypeLevel.List.:::"
tnil = ConT $ mkName "Data.TypeLevel.List.Nil"
tfalse = ConT $ mkName "Data.TypeLevel.Bool.False"                  
ttrue = ConT $ mkName "Data.TypeLevel.Bool.True"                  

        
resultsFalseFor q = return $ map resultFalse [1..q-1]
    where resultFalse p = InstanceD [] (tcurry tdivides [tdigit q, tcurry tcons [tdigit p, tnil], tfalse]) []

resultsFalse k = liftM concat $ mapM resultsFalseFor [2..k]
