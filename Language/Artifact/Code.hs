{-# LANGUAGE TypeOperators, FlexibleInstances,
  MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module Language.Artifact.Code where

-- import Control.Monad.Reader
-- import Control.Monad.Identity

-- data Environment = Environment
--   { getEnv :: String
--   }

-- type Generator = ReaderT Environment Identity

-- runGenerator :: Generator a -> Environment -> a
-- runGenerator c env = runIdentity (runReaderT c env)

class Code a b  {-| a -> b -} where
  code :: a -> b
