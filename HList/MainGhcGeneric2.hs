{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Included for completeness' sake.
   The TypeEqBoolGeneric2.hs implementation is demonstrated.

-}

module HList.MainGhcGeneric2 where

import HList.FakePrelude hiding (TypeEq,typeEq,proxyEq)
import HList.TypeEqGeneric2


{-----------------------------------------------------------------------------}

main = print ( typeEq True False
             , typeEq True "True"
             )


{-----------------------------------------------------------------------------}
