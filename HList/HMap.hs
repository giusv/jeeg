{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}
module HList.HMap where
import HList.HBasic
import HList.HApply
import HList.HFoldr

data HMap f = HMap f

hMap f = hFoldr (HMap f) hNil 

instance HApply (HMap f) HNil HNil
      where
      hApply _ _ = hNil
instance HApply f e e'
      => HApply (HMap f) (e,l) (HCons e' l)
    where
    hApply (HMap f) (e,l) = HCons e' l
        where e' = hApply f e


    
    
-- class HList l => HMap f l r | f l -> r where
    -- hMap :: f -> l -> r

-- instance HMap f HNil HNil where
    -- hMap _ _ = HNil

-- instance ( HMap f l r
         -- , HApply f e e'
         -- ) => HMap f (HCons e l) (HCons e' r) where
    -- hMap f (HCons e l) = HCons (hApply f e) (hMap f l)

    