-- {-# OPTIONS -fglasgow-exts #-}
-- {-# OPTIONS -fallow-undecidable-instances #-}
{-# LANGUAGE UndecidableInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances,FlexibleContexts #-}
{- 

   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   A generic implementation of type cast. For this implementation to
   work, we need to import HList.it at a higher level in the module HList.hierarchy
   than all clients of the class. Otherwise, type simplification will
   inline TypeCast x y, which implies compile-time unification of x and y.

   This technique works fine for ghc, and within limits for hugs.

-}

  
module HList.TypeCastGeneric1 where

import HList.FakePrelude

instance TypeCast x x
 where
  typeCast = id
