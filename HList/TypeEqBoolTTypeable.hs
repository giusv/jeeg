{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Implementations of type equality and disequality based on TTypeable.
   This approach works for GHC and Hugs.

-}

module HList.TypeEqBoolTTypeable where

import HList.FakePrelude
import HList.TTypeable
import HList.TypeEqTTypeable

instance TypeEq x y HTrue  => TypeEqTrue x y
instance TypeEq x y HFalse => TypeEqFalse x y
