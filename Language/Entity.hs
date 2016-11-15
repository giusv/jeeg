{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}

module Language.Entity where
import Data.List
data Attribute t name
attr = undefined :: Attribute t name



data PEOPLE a; people = undefined :: PEOPLE ()
data ID; atID = attr :: Attribute Int (PEOPLE ID); instance Show (Attribute Int (PEOPLE ID)) where show _ = "ID"
data NAME; atName = attr :: Attribute String (PEOPLE NAME); instance Show (Attribute String (PEOPLE NAME)) where show _ = "NAME"
data AGE; atAge = attr :: Attribute String (PEOPLE AGE); instance Show (Attribute String (PEOPLE AGE)) where show _ = "AGE"
data CITY; atCity = attr :: Attribute String (PEOPLE CITY); instance Show (Attribute String (PEOPLE CITY)) where show _ = "CITY"

-- Heterogeneous type sequences
data HNil      = HNil      deriving (Eq,Show,Read,Ord)
data HCons e l = HCons e l deriving (Eq,Show,Read,Ord)

-- The set of all types of heterogeneous lists
class HList l
instance HList HNil
instance HList l => HList (HCons e l)

-- Public constructors
hNil  :: HNil
hNil  =  HNil

hCons :: HList l => e -> l -> HCons e l
hCons e l = HCons e l

infixr 2 :*:
infixr 2 .*.

type (:*:) e l = HCons e l
e .*. l = HCons e l
l .=. v = (l, v)

class (HList l) => HShow l where
    hShow :: l -> [String]
    
instance HShow HNil where
    hShow HNil = []
instance (Show e, HShow l) => HShow (HCons e l) where
    hShow (HCons e l) = show e : hShow l



