{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Attribute where
import Language.Commons
import HList.HBasic
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad

data EFields = EFields {eFields :: [Decl]}
data EAccessors = EAccessors {eAccessors :: [Decl]}


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

