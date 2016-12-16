{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Entity where
import Language.Commons
import HList.HBasic
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad


data Attribute t name
attr = undefined :: Attribute t name

instance Show (Attribute t name) => Code (Attribute t name) EntityFields where
    code a = do 
        let name = show a
        return $ 
            EntityFields [FieldDecl
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
                   [VarDecl (VarId (Ident name)) Nothing]]
                       
instance Show (Attribute t name) => Code (Attribute t name) Accessors where
    code a = do 
        let name = show a
        return $ 
            Accessors [MemberDecl
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
                       (Ident "set" ++ capitalize name)
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

data EntityFields = EntityFields [MemberDecl]
data Accessors = Accessors [MemberDecl]

-- data EntitySetter   = EntitySetter  MemberDecl
-- data EntityGetter   = EntityGetter  MemberDecl

instance Code HNil EntityFields where
    code _ = return []
    
instance (Code e EntityFields, Code l EntityFields) => Code (HCons e l) EntityFields where
    code (HCons e l) = liftM2 (++) (code e) (code l)

instance Code HNil Accessors where
    code _ = return []
    
instance (Code e Accessors, Code l Accessors) => Code (HCons e l) Accessors where
    code (HCons e l) = liftM2 (++) (code e) (code l)


instance (Code k EntityFields, 
          Code a EntityFields, 
          Show e) 
          => Code (Entity e k a) CompilationUnit where
    code (Entity e k a) = do 
        (EntityFields k') <- code k 
        (EntityFields a') <- code a
        (Accessors k'') <- code k
        (Accessors a'') <- code a
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
                         (ClassBody $ 
                                map k'
                            ++  map a'
                            ++  map k''
                            ++  map a''))
                  ]
    