{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Entity where
import Language.Primitive.Attribute
import Language.Commons
-- import MyHList.HBasic
import HList.CommonMain
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad

data Entity e a c = Entity e a c

instance (Show e) => Show (Entity e a c) where
    show (Entity e _ _) = show e

entity :: (HList a, HList c) => e -> a -> c -> Entity e a c
entity e a c = Entity e a c

instance Code HNil EMembers where
    code _ = []
    
instance (Code v EMembers, Code l EMembers) => Code (HCons (k,v) l) EMembers where
    code (HCons (k,v) l) =  code v ++ code l


instance (Code a EMembers, Show e) 
          => Code (Entity e a c) CompilationUnit where
    code (Entity e a c) = 
      let a' = code a
        in CompilationUnit
          (Just
             (PackageDecl (Name [Ident "com", Ident "example"])))
          [ ImportDecl
              False
              (Name [Ident "javax", Ident "persistence", Ident "Column"])
              False
          , ImportDecl
              False
              (Name [Ident "javax", Ident "persistence", Ident "Entity"])
              False
          , ImportDecl
              False
              (Name [Ident "javax", Ident "persistence", Ident "Table"])
              False
          ]
          [ ClassTypeDecl
              (ClassDecl
                 [ Annotation (MarkerAnnotation {annName = Name [Ident "Entity"]})
                 , Annotation
                     (NormalAnnotation
                      { annName = Name [Ident "Table"]
                      , annKV = [(Ident "name", EVVal (InitExp (Lit (String (show e)))))]
                      })
                 , Public
                 ]
                 (Ident $ capitalize (show e))
                 []
                 Nothing
                 []
                 (ClassBody a'))
          ]
        
