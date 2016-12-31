{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}
module MyHList.HFoldr where
import MyHList.HBasic
import MyHList.HApply

class HList l => HFoldr f v l r | f v l -> r where
    hFoldr :: f -> v -> l -> r

instance HFoldr f v HNil v where
    hFoldr _ v _ = v

instance (HFoldr f v l r, HApply f (e,r) r')
       => HFoldr f v (HCons e l) r' where
    hFoldr f v (HCons e l) = hApply f (e, hFoldr f v l)

-- hAppend :: HFoldr ApplyHCons v l r => l -> v -> r
-- hAppend l v = hFoldr ApplyHCons v l


-- hMap :: (HFoldr f HNil l r) => f -> l -> r

-- hMap' :: ( HApply f e e'
         -- , HFoldr ((e, a) -> HCons e' a) HNil l r
         -- , HList l
         -- ) => f -> l -> r
-- hMap' f l = hFoldr step HNil l 
           -- where step (e,a) = hApply ApplyHCons (hApply f e,a)
           
-- -- hMap f l = hFoldr (hApply ApplyHCons (hApply f)) HNil l
