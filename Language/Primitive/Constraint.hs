{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Constraint where
import Language.Primitive.Expression
import Language.Primitive.Attribute
import Language.Commons
import Language.Java.Syntax
import HList.CommonMain
import HList.GhcSyntax
import Language.Artifact.Code

class Constraints a
instance Constraints HNil
instance (Constraints l) => Constraints (HCons (PrimaryKey pk) l)
instance (Constraints l) => Constraints (HCons (ForeignKey fk t pk) l)


data PrimaryKey pk = PrimaryKey pk
pk :: (HList pk) => pk -> PrimaryKey pk
pk k = PrimaryKey k

data ForeignKey fk t pk = ForeignKey fk t pk Cardinality
fk :: (HList pk,HList fk) => pk -> t -> fk -> Cardinality -> ForeignKey pk t fk
fk k t pk card = ForeignKey k t pk card

data Cardinality = OneToOne
                 | OneToMany
                 | ManyToOne
                 deriving (Show)


-- class Constraints a
-- instance Constraints HNil
-- instance (Constraints l) => Constraints (HCons (PrimaryKey pk) l)
-- instance (Constraints l) => Constraints (HCons (ForeignKey fk t pk) l)


-- data PrimaryKey pk = PrimaryKey pk
-- pk :: (HList pk) => pk -> PrimaryKey pk
-- pk k = PrimaryKey k

-- data ForeignKey fk t pk = ForeignKey fk t pk Cardinality
-- fk :: (HList pk,HList fk) => pk -> t -> fk -> Cardinality -> ForeignKey pk t fk
-- fk k t pk card = ForeignKey k t pk card
