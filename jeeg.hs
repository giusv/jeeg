-- /d/Dati/Profili/M026980/Documents/programmi/jeeg
{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,UndecidableInstances #-}
module Jeeg where

import Language.Entity
import Generator.Jpa
import Generator.Commons
import Language.Java.Pretty

peopleSchema = (atID .*. HNil, atName .*. atAge .*. atCity .*. HNil)

main = do
    putStrLn $ prettyPrint $ runGenerator (entity peopleSchema) (Environment "fff")
    
-- import HList.CommonMain
-- import Data.Map
-- import HList.GhcSyntax
-- import HList.Label3
-- import HList.TypeEqBoolGeneric
-- import HList.TypeEqGeneric1
-- import HList.TypeCastGeneric1



-- newtype Key = Key Integer deriving (Show,Eq,Ord)
-- newtype Name = Name String deriving (Show,Eq)
-- data Breed = Cow | Sheep deriving (Show,Eq)
-- newtype Price = Price Float deriving (Show,Eq,Ord)
-- data Disease = BSE | FM deriving (Show,Eq)


-- data FootNMouth = FootNMouth -- a namespace

-- key = firstLabel FootNMouth "key"
-- name = nextLabel key "name"
-- breed = nextLabel name "breed"
-- price = nextLabel breed "price"


-- l .!. v = hLookupByLabel l v
-- myRecord = Record (    hZero .=. "foo" 
                   -- .*. hOne  .=. True 
                   -- .*. HNil)
-- hOne = hSucc hZero

-- -----------------------------------------------------------------------------
-- -- * General type-level functionality.

-- -- | Type-level predicate that tests that no type is repeated
-- --   in a list.
-- class NoRepeats l where
    -- noRepeats :: l -> Bool
-- instance 
      -- NoRepeats HNil where
    -- noRepeats _ = True
-- instance (
      -- HTMember e l b,
      -- BooleanValue b,
      -- NoRepeats l
 -- ) => NoRepeats (HCons e l) where
    -- noRepeats (HCons e l) = (not $ booleanValue $ hTMember e l) && (noRepeats l)



-- -- | Convert type-level booleans to value-level booleans.
-- class HBool b => BooleanValue b where
    -- booleanValue :: b -> Bool
-- instance BooleanValue HTrue where
    -- booleanValue _ = True
-- instance BooleanValue HFalse where
    -- booleanValue _ = False 

    
    
-- data HeaderFor h k v =>  Table h k v = Table h (Map k v)
-- class HeaderFor h k v | h -> k v
-- instance (
    -- AttributesFor a k, AttributesFor b v,
    -- HAppend a b ab, NoRepeats ab, Ord k) => HeaderFor (a, b) k v

-- data Attribute t name
-- attr = undefined :: Attribute t name
-- class AttributesFor a v | a -> v
-- instance AttributesFor HNil HNil
-- instance AttributesFor a v
    -- => AttributesFor (HCons (Attribute t name) a) (HCons t v)

    
-- data ID; atID = attr :: Attribute Int (PEOPLE ID)
-- data NAME; atName = attr :: Attribute String (PEOPLE NAME)
-- data AGE; atAge = attr :: Attribute String (PEOPLE AGE)
-- data CITY; atCity = attr :: Attribute String (PEOPLE CITY)
-- data PEOPLE a; people = undefined :: PEOPLE ()

-- -- table :: HeaderFor h k v => h -> Map k v -> Table h k v
-- -- table h m = Table h m

-- myHeader = (atID .*. HNil, atName .*. atAge .*. atCity .*. HNil)

-- myTable = Table myHeader $
    -- insert (12 .*. HNil) ("Ralf" .*. 23 .*. "Seattle" .*. HNil) $
    -- insert (67 .*. HNil) ("Oleg" .*. 17 .*. "Seattle" .*. HNil) $
    -- insert (50 .*. HNil) ("Dorothy" .*. 42 .*. "Oz" .*. HNil) $
    -- empty

-- instance ShowLabel HZero where
    -- showLabel x = show x
    
-- instance ShowLabel (HSucc HZero)
 -- where showLabel x = show x
 
-- instance (HNat n, Show n) => ShowLabel (HSucc n)
 -- where showLabel n = show n
 
 
-- unpricedAngus = key .=. (42::Integer)
            -- .*. name .=. "Angus"
            -- .*. breed .=. Cow
            -- .*. emptyRecord

-- e1 = Entity "user" []
-- er = ER [("users",e1)] []
-- main :: IO ()
-- main = do
    -- let target = "D:/Dati/temp"
    -- let out = generate er
    -- -- putStrLn $ render $ getDoc out
    -- mapM_ (saveFile target) out
    -- return ()
    
   