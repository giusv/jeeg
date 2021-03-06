{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies #-}
module Jeeg where

import Language
-- import MyHList.HBasic
import HList.CommonMain
import HList.GhcSyntax
import HList.Label2
import Language.Java.Syntax
import Language.Java.Pretty

data PEOPLE a
people = undefined :: PEOPLE ()
instance Show (PEOPLE ()) where show _ = "PEOPLE"

data ID
atID = attr :: Attribute Int (PEOPLE ID) 
instance ShowAttribute (Attribute Int (PEOPLE ID)) where
  showName _ = "ID"
  showType _ = "int"
instance Show (Attribute Int (PEOPLE ID)) where
  show _ = "PEOPLE.ID"

data NAME
atName = attr :: Attribute String (PEOPLE NAME)
instance ShowAttribute (Attribute String (PEOPLE NAME)) where
  showName _ = "NAME"
  showType _ = "String"
instance Show (Attribute String (PEOPLE NAME)) where
  show _ = "PEOPLE.NAME"

data AGE
atAge = attr :: Attribute Int (PEOPLE AGE)
instance ShowAttribute (Attribute Int (PEOPLE AGE)) where
  showName _ = "AGE"
  showType _ = "int"
instance Show (Attribute Int (PEOPLE AGE)) where
  show _ = "PEOPLE.AGE"

data CITY
atCity = attr :: Attribute String (PEOPLE CITY)
instance ShowAttribute (Attribute String (PEOPLE CITY)) where
  showName _ = "CITY"
  showType _ = "String"

instance Show (Attribute String (PEOPLE CITY)) where
  show _ = "PEOPLE.ID"

data CITIES a
cities = undefined :: CITIES ()
cityID = attr :: (Attribute Int (CITIES ID))
instance ShowAttribute (Attribute Int (CITIES ID)) where
  showName _ = "ID"
  showType _ = "int"

instance Show (Attribute Int (CITIES ID)) where
  show _ = "CITIES.ID"

cityName = attr :: (Attribute String (CITIES NAME))
instance ShowAttribute (Attribute String (CITIES NAME)) where
  showName _ = "NAME"
  showType _ = "String"
instance Show (Attribute String (CITIES NAME)) where
  show _ = "CITIES.NAME"

data People = People
lid = firstLabel People "id"
lname = nextLabel lid "name"
lage = nextLabel lname "age"
lcity = nextLabel lage "city"

peopleEntity = entity people peopleAttrs peopleConstr
peopleAttrs =  lid .=. atID
               .*. lname .=. atName
               .*. lage .=. atAge
               .*. lcity .=. atCity
               .*. HNil
peopleConstr = pk (lid .*. lname .*. HNil)
               .*. fk (lcity .*. HNil) citiesAttrs (cid .*. HNil) OneToMany
               .*. HNil

data Cities = Cities
cid = firstLabel Cities "id"
cname = nextLabel cid "name"
citiesAttrs = cid .=. cityID
            .*. cname .=. cityName
            .*. HNil
citiesEntity = entity cities citiesAttrs HNil

main :: IO ()
main = do
    putStrLn $ prettyPrint $ ((code peopleEntity) :: CompilationUnit)
    return ()

query =
  let (TableQ p) = table peopleEntity
      (TableQ c) = table citiesEntity
     -- test = p ! lcity
  in restrict ((fromAttribute (p .!. lcity)) *==* (fromAttribute (c .!. cname))) ((TableQ p) `times` (TableQ c))

test2 =
  let p = peopleAttrs
      c = citiesAttrs
      in ((fromAttribute (p .!. lcity)) *==* (fromAttribute (c .!. cname)))
  
