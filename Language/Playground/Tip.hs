{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Playground.Tip where

data Tip a = Tip a

instance (Show a) => Show (Tip a) where
  show (Tip a) = "Tip " ++ show a



