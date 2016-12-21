{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Playground.Fork where

data Fork a b = Fork a b

instance (Show a, Show b) => Show (Fork a b) where
  show (Fork l r) = "Fork (" ++ show l ++ ") (" ++ show r ++ ")"
