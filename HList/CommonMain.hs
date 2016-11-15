-- {-# OPTIONS -fglasgow-exts #-}
-- {-# OPTIONS -fallow-undecidable-instances #-}
-- {-# OPTIONS -fallow-overlapping-instances #-}

{-# LANGUAGE UndecidableInstances #-}

{-
   
   The HList library

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   This is a next-to-main module HList.that loads all modules that at least
   *compile* fine for all the models of interest. See the Makefile
   for ways to run different models.

-}

module HList.CommonMain (

   module HList.FakePrelude
 , module HList.HListPrelude
 , module HList.HArray
 , module HList.HOccurs
 , module HList.HTypeIndexed
 , module HList.TIP
 , module HList.TIC
 , module HList.HZip
 , module HList.Record
 , module HList.Variant
) where

import HList.FakePrelude
import HList.HListPrelude
import HList.HArray
import HList.HOccurs
import HList.HTypeIndexed
import HList.TIP
import HList.TIC
import HList.HZip
import HList.Record
import HList.Variant
