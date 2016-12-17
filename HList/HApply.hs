{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}
module HList.HApply where
import HList.HBasic

class HApply f a r | f a -> r where
    hApply :: f -> a -> r
    
data ApplyHCons = ApplyHCons -- a proxy for instance selection
instance HApply ApplyHCons (e,l) (HCons e l) where
    hApply _ (e,l) = HCons e l

-- Normal function application

instance HApply (x -> y) x y where
    hApply f x = f x


-- Identity

data Id = Id

instance HApply Id x x  where
    hApply _ x = x

data Shw = Shw
instance (Show x) => HApply Shw x [Char] where
    hApply _ x = show x
