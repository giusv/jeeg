{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}
module HList.HBasic where

-- Heterogeneous type sequences
data HNil      = HNil      deriving (Eq,Show,Read,Ord)
data HCons e l = HCons e l deriving (Eq,Show,Read,Ord)

-- The set of all types of heterogeneous lists
class HList l
instance HList HNil
instance HList l => HList (HCons e l)

-- Public constructors
hNil :: HNil
hNil = HNil

hCons :: HList l => e -> l -> HCons e l
hCons e l = HCons e l

infixr 2 :*:
infixr 2 .*.

type (:*:) e l = HCons e l
e .*. l = HCons e l
l .=. v = (l, v)


