{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}
module HList.HFoldr where
import HList.HBasic
import HList.HApply

class HList l => HFoldr f v l r | f v l -> r where
    hFoldr :: f -> v -> l -> r

instance HFoldr f v HNil v where
    hFoldr _ v _ = v

instance (HFoldr f v l r, HApply f (e,r) r')
       => HFoldr f v (HCons e l) r' where
    hFoldr f v (HCons e l) = hApply f (e, hFoldr f v l)
    
hAppend l l' = hFoldr ApplyHCons l' l

-- hMap f l = hFoldr step HNil l 
           -- where step (e,a) = hApply ApplyHCons (hApply f e)
           
hMap f l = hFoldr (hApply ApplyHCons (hApply f)) HNil l 
