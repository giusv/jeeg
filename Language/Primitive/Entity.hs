{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Entity where
import Language.Primitive.Attribute
import Language.Primitive.Constraint
import Language.Commons
-- import MyHList.HBasic
import HList.CommonMain
import HList.GhcSyntax
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad

data Entity e a = Entity e a

instance (Show e) => Show (Entity e a) where
    show (Entity e _) = show e

entity :: (Attributes a) => e -> a -> Entity e a
entity e a = Entity e a

instance Code HNil EMembers where
    code _ = []
    
instance (Code v EMembers, Code l EMembers) => Code (HCons (k :=: v) l) EMembers where
    code (HCons (k,v) l) =  code v ++ code l

instance (Code a EMembers, Show e) 
          => Code (Entity e a) CompilationUnit where
    code (Entity e a) = 
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
        
