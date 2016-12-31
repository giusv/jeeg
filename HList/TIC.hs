-- {-# OPTIONS -fglasgow-exts #-}
-- {-# OPTIONS -fallow-undecidable-instances #-}
-- {-# OPTIONS -fallow-overlapping-instances #-}
{-# LANGUAGE UndecidableInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,FlexibleContexts #-}
{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

-}


module HList.TIC where

import Data.Typeable hiding (Proxy)
import Data.Dynamic hiding (Proxy)
import HList.FakePrelude
import HList.HListPrelude
import HList.HOccurs
import HList.HTypeIndexed
import HList.TIP


{-----------------------------------------------------------------------------}

-- A datatype for type-indexed co-products

data TIC l = TIC Dynamic


{-----------------------------------------------------------------------------}

-- Public constructor

mkTIC :: ( HTypeIndexed l
         , HTypeProxied l
         , HOccurs (Proxy i) l
         , Typeable i
         ) 
      => i -> TIC l

mkTIC i = TIC (toDyn i)


{-----------------------------------------------------------------------------}

-- Public destructor

unTIC :: ( HTypeIndexed l
         , HTypeProxied l
         , HOccurs (Proxy o) l
         , Typeable o
         ) 
      => TIC l -> Maybe o

unTIC (TIC i) = fromDynamic i


{-----------------------------------------------------------------------------}

-- A type-indexed type sequence that is a sequence of proxy types

class HTypeProxied l
instance HTypeProxied HNil
instance HTypeProxied l => HTypeProxied (HCons (Proxy e) l)


{-----------------------------------------------------------------------------}

-- TICs are opaque

instance Show (TIC l)
 where
  show _ = "<Cannot show TIC content!>"


{-----------------------------------------------------------------------------}
