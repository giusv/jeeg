{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Included for completeness' sake.
   The TypeCastGeneric2.hs implementation is demonstrated.

-}

module HList.MainGhcGeneric3 where

import HList.FakePrelude hiding (TypeCast,typeCast)
import HList.TypeCastGeneric2


{-----------------------------------------------------------------------------}

main = print ( typeCast 1
             , typeCast True
             )


{-----------------------------------------------------------------------------}
