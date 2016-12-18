{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Entity where
import Language.Commons
import HList.HBasic
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad


data Attribute t name
attr = undefined :: Attribute t name

instance Show (Attribute t name) => Code (Attribute t name) EFields where
    code a = do 
        let name = show a
        return $ 
            EFields 
                [ MemberDecl 
                    (FieldDecl
                       [ Annotation
                           (NormalAnnotation
                            { annName = Name [Ident "Column"]
                            , annKV =
                                [ ( Ident "name"
                                  , EVVal (InitExp (Lit (String name))))
                                ]
                            })
                       ]
                       (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                       [VarDecl (VarId (Ident name)) Nothing])]
                           
instance Show (Attribute t name) => Code (Attribute t name) EAccessors where
    code a = do 
        let name = show a
        return $ 
            EAccessors 
                [ MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       (Just
                          (RefType (ClassRefType (ClassType [(Ident "String", [])]))))
                       (Ident $ "get" ++ capitalize name)
                       []
                       []
                       (MethodBody
                          (Just
                             (Block
                                [ BlockStmt
                                    (Return (Just (ExpName (Name [Ident name]))))
                                ]))))
                , MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       Nothing
                       (Ident $ "set" ++ capitalize name)
                       [ FormalParam
                           []
                           (RefType
                              (ClassRefType (ClassType [(Ident "String", [])])))
                           False
                           (VarId (Ident name))
                       ]
                       []
                       (MethodBody
                          (Just
                             (Block
                                [ BlockStmt
                                    (ExpStmt
                                       (Assign
                                          (FieldLhs
                                             (PrimaryFieldAccess
                                                This
                                                (Ident name)))
                                          EqualA
                                          (ExpName (Name [Ident name]))))
                                ]))))
                ]

data Entity e k a = Entity e k a

instance (Show e) => Show (Entity e k a) where
    show (Entity e _ _) = show e

entity :: (HList k, HList a) => e -> k -> a -> Entity e k a
entity e k a = Entity e k a

data EFields = EFields {eFields :: [Decl]}
data EAccessors = EAccessors {eAccessors :: [Decl]}

-- data EntitySetter   = EntitySetter  MemberDecl
-- data EntityGetter   = EntityGetter  MemberDecl

instance Code HNil EFields where
    code _ = return $ EFields []
    
instance (Code e EFields, Code l EFields) => Code (HCons e l) EFields where
    code (HCons e l) = liftM EFields (liftM2 (++) (liftM eFields (code e)) (liftM eFields (code l)))

instance Code HNil EAccessors where
    code _ = return $ EAccessors []
    
instance (Code e EAccessors, Code l EAccessors) => Code (HCons e l) EAccessors where
    code (HCons e l) = liftM EAccessors (liftM2 (++) (liftM eAccessors (code e)) (liftM eAccessors (code l)))


instance (Code k EFields, Code k EAccessors,
          Code a EFields, Code a EAccessors,
          Show e) 
          => Code (Entity e k a) CompilationUnit where
    code (Entity e k a) = do 
            (EFields k') <- code k 
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
        