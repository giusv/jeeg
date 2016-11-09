module Language.Entity
  ( ER(..)
  , Entity(..)
  , Attribute(..)
  , Relation(..)
  ) where

import Data.Text
data ER = ER { entities :: [Entity]
             , rels :: [Relation]
             }
          deriving Show

data Entity = Entity { name :: Text
                     , attribs :: [Attribute]
                     }
              deriving Show

instance Eq Entity where
  e1 == e2 = name e1 == name e2

instance Ord Entity where
  e1 `compare` e2 = name e1 `compare` name e2

-- | Represents a single attribute in a particular entity.
data Attribute = Attribute { field :: Text
                           , pk :: Bool
                           , fk :: Bool
                           }
                 deriving Show

instance Eq Attribute where
  a1 == a2 = field a1 == field a2

instance Ord Attribute where
  a1 `compare` a2 = field a1 `compare` field a2

-- Each relationship has one of four cardinalities specified for both entities.
-- Those cardinalities are: 0 or 1, exactly 1, 0 or more and 1 or more.
data Relation = Relation { entity1, entity2 :: Text
                         , card1,   card2   :: Cardinality
                         }
                deriving Show

data Cardinality = ZeroOne
                 | One
                 | ZeroPlus
                 | OnePlus

instance Show Cardinality where
  show ZeroOne = "{0,1}"
  show One = "1"
  show ZeroPlus = "0..N"
  show OnePlus ="1..N"
