module Language.Entity
  ( ER(..)
  , Entity(..)
  , Attr(..)
  , Relation(..)
  ) where

-- import Data.Text
data ER = ER { entities :: [(String,Entity)]
             , rels :: [Relation]
             }
          deriving (Show)

data Entity = Entity { title :: String
                     , attribs :: [Attr]
                     }
              deriving (Show)
              
primary :: Entity -> [Attr]
primary e = filter (\a -> pk a == True) (attribs e)

extern :: Entity -> [Attr]
extern e = filter (\a -> fk a == True) (attribs e)

instance Eq Entity where
    e1 == e2 = title e1 == title e2

instance Ord Entity where
    e1 `compare` e2 = title e1 `compare` title e2

data Category = Number
              | Text
              | Blob
              deriving (Show)
              
-- | Represents a single Attr in a particular entity.
data Attr = Attr { field :: String
                           , category :: Category
                           , pk :: Bool
                           , fk :: Bool
                           }
                 deriving (Show)

instance Eq Attr where
    a1 == a2 = field a1 == field a2

instance Ord Attr where
    a1 `compare` a2 = field a1 `compare` field a2

-- Each relationship has one of four cardinalities specified for both entities.
-- Those cardinalities are: 0 or 1, exactly 1, 0 or more and 1 or more.
data Relation = Relation { entity1, entity2 :: String
                         , card1,   card2   :: Cardinality
                         }
                deriving (Show)

data Cardinality = ZeroOne
                 | One
                 | ZeroPlus
                 | OnePlus

instance Show Cardinality where
    show ZeroOne = "{0,1}"
    show One = "1"
    show ZeroPlus = "0..N"
    show OnePlus ="1..N"
