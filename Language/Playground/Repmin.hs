{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Playground.Repmin where
import Language.Playground.Tip
import Language.Playground.Fork

class Repmin a b where
  repmin :: a -> (b -> (a,b))

instance (Ord a) => Repmin (Tip a) a where
  repmin (Tip i) m =
    let tmin = i
        ttree = Tip m
    in (ttree, tmin)

    
instance (Ord a, Repmin l a, Repmin r a) => Repmin (Fork l r) a where
  repmin (Fork l r) trep =
    let (ltree,lmin) = repmin l lrep
        (rtree,rmin) = repmin r rrep
        tmin = min lmin rmin
        lrep = trep
        rrep = trep
        ttree = Fork ltree rtree
    in (ttree, tmin)
top :: (Repmin t Integer) => t -> t
top t =
  let (ttree, tmin) = repmin t trep
      trep = tmin :: Integer
  in ttree
main =
  let t = Fork (Tip (2 :: Integer)) (Tip (1 :: Integer))
  in top t
