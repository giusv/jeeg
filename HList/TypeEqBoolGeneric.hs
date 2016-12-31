-- {-# OPTIONS -fglasgow-exts #-}
-- {-# OPTIONS -fallow-undecidable-instances #-}
-- {-# OPTIONS -fallow-overlapping-instances #-}
{-# LANGUAGE UndecidableInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,FlexibleContexts #-}

{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Generic implementations of type equality and disequality

-}

module HList.TypeEqBoolGeneric where

import HList.FakePrelude

instance            TypeEqTrue  x x
instance Fail () => TypeEqFalse x x
instance            TypeEqFalse x y
