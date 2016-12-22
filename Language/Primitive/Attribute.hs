{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Attribute where
import Language.Commons
import HList.HBasic
import Language.Java.Syntax
import Language.Artifact.Code
import Control.Monad

data EFields = EFields {eFields :: [Decl]}
data EAccessors = EAccessors {eAccessors :: [Decl]}

class ShowAttribute a where
  showName :: a -> String
  showType :: a -> String


data Attribute t name
attr = undefined :: Attribute t name

instance ShowAttribute (Attribute t name) => Code (Attribute t name) ([Modifier] -> EFields) where
    code a anns = do 
        let nm = showName a
        let tp = showType a
        return $ 
            EFields 
                [ MemberDecl 
                    (FieldDecl
                       anns ++ 
                       [ Annotation
                           (NormalAnnotation
                            { annName = Name [Ident "Column"]
                            , annKV =
                                [ ( Ident "name"
                                  , EVVal (InitExp (Lit (String $ upperCase nm))))
                                ]
                            })
                       ]
                       (RefType (ClassRefType (ClassType [(Ident tp, [])])))
                       [VarDecl (VarId (Ident $ lowerCase nm)) Nothing])]
                           
instance ShowAttribute (Attribute t name) => Code (Attribute t name) EAccessors where
    code a = do 
        let nm = showName a
        let tp = showType a
        return $ 
            EAccessors 
                [ MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       (Just
                          (RefType (ClassRefType (ClassType [(Ident $ tp, [])]))))
                       (Ident $ "get" ++ capitalize nm)
                       []
                       []
                       (MethodBody
                          (Just
                             (Block
                                [ BlockStmt
                                    (Return (Just (ExpName (Name [Ident $ lowerCase nm]))))
                                ]))))
                , MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       Nothing
                       (Ident $ "set" ++ capitalize nm)
                       [ FormalParam
                           []
                           (RefType
                              (ClassRefType (ClassType [(Ident tp, [])])))
                           False
                           (VarId (Ident $ lowerCase nm))
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
                                                (Ident $ lowerCase nm)))
                                          EqualA
                                          (ExpName (Name [Ident $ lowerCase nm]))))
                                ]))))
                ]

