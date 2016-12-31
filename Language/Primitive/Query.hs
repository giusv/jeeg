{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts, UndecidableInstances #-}

module Language.Primitive.Query where
import Language.Primitive.Attribute
import Language.Primitive.Expression
import Language.Primitive.Entity
import Language.Commons
import MyHList.HBasic
import MyHList.HAppend
import Control.Monad.State

-- entity
data EmptyQ = EmptyQ deriving (Show)
data TableQ e = TableQ e deriving (Show)
data ProjectQ a q = ProjectQ a q deriving (Show)
data RestrictQ e q = RestrictQ e q deriving (Show)
data TimesQ q1 q2 = TimesQ q1 q2 deriving (Show)

class (HList l) => Scheme a l | a -> l where
  scheme :: a -> l

instance Scheme EmptyQ HNil where
  scheme _ = HNil

instance (HList e) => Scheme (TableQ e) e where
  scheme (TableQ e) = e

instance (HList a) => Scheme (ProjectQ a q) a where
  scheme (ProjectQ a q) = a

instance (Scheme q s) => Scheme (RestrictQ e q) s where
  scheme (RestrictQ e q) = scheme q

instance (Scheme q1 s1, Scheme q2 s2, HAppend s1 s2 s, HList s) => Scheme (TimesQ q1 q2) s where
  scheme (TimesQ q1 q2) = hAppend (scheme q1) (scheme q2)

table :: Entity e a c -> TableQ a
table (Entity _ a _) = TableQ a

restrict :: Expression Bool a -> q -> RestrictQ (Expression Bool a) q
restrict e q = RestrictQ e q

project :: (HList a) => a -> q -> ProjectQ a q
project a q = ProjectQ a q

times :: q1 -> q2 -> TimesQ q1 q2
times q1 q2 = TimesQ q1 q2

