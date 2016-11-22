{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts,UndecidableInstances #-}

module Language.Entity where
import Data.List
import HList.HShow
data Attribute t name
attr = undefined :: Attribute t name



data PEOPLE a; people = undefined :: PEOPLE (); instance Show (PEOPLE ()) where show _ = "PEOPLE"
data ID; atID = attr :: Attribute Int (PEOPLE ID); instance Show (Attribute Int (PEOPLE ID)) where show _ = "ID"
data NAME; atName = attr :: Attribute String (PEOPLE NAME); instance Show (Attribute String (PEOPLE NAME)) where show _ = "NAME"
data AGE; atAge = attr :: Attribute String (PEOPLE AGE); instance Show (Attribute String (PEOPLE AGE)) where show _ = "AGE"
data CITY; atCity = attr :: Attribute String (PEOPLE CITY); instance Show (Attribute String (PEOPLE CITY)) where show _ = "CITY"
