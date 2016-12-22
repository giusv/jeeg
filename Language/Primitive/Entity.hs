{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Entity where
import Language.Primitive.Attribute
import Language.Commons
import HList.HBasic
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad

data Entity e k a = Entity e k a

instance (Show e) => Show (Entity e k a) where
    show (Entity e _ _) = show e

entity :: (HList k, HList a) => e -> k -> a -> Entity e k a
entity e k a = Entity e k a

instance Code HNil ([Modifier] -> EFields) where
    code _ _ = return $ EFields []
    
instance (Code e ([Modifier] -> EFields), Code l ([Modifier] -> EFields)) => Code (HCons e l) ([Modifier] -> EFields) where
    code (HCons e l) ann = liftM EFields (liftM2 (++) (liftM eFields (code e ann)) (liftM eFields (code l ann)))

instance Code HNil EAccessors where
    code _ = return $ EAccessors []
    
instance (Code e EAccessors, Code l EAccessors) => Code (HCons e l) EAccessors where
    code (HCons e l) = liftM EAccessors (liftM2 (++) (liftM eAccessors (code e)) (liftM eAccessors (code l)))


instance (Code k ([Modifier] -> EFields), Code k EAccessors,
          Code a ([Modifier] -> EFields), Code a EAccessors,
          Show e) 
          => Code (Entity e k a) CompilationUnit where
    code (Entity e k a) = do
      let anns = [MarkerAnnotation {annName = Name [Ident "Id"]}]
            (EFields k') <- code k anns
            (EFields a') <- code a 
            (EAccessors k'') <- code k
            (EAccessors a'') <- code a
            return $ CompilationUnit
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
                             (ClassBody $ k' ++ a' ++ k''++ a''))
                      ]
        
