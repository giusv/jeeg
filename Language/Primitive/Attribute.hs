{-#LANGUAGE TypeOperators,FlexibleInstances,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts #-}

module Language.Primitive.Attribute where
import Language.Primitive.Expression
import Language.Commons
--import MyHList.HBasic
import Language.Java.Syntax
import HList.CommonMain
import HList.GhcSyntax
import Language.Artifact.Code
import Control.Monad

type EMembers = [Decl]

class ShowAttribute a where
  showName :: a -> String
  showType :: a -> String


data Attribute t name
attr = undefined :: Attribute t name

class Attributes a
instance Attributes HNil
instance (Attributes l) => Attributes (HCons (n :=: a) l)


data AttrExpr t name = AttrExpr (Attribute t name)
--                     deriving (Show)
fromAttribute :: Attribute t name -> Expression t (AttrExpr t name)
fromAttribute attr = Expression (AttrExpr attr)


instance ShowAttribute (Attribute t name) => Code (Attribute t name) EMembers where
    code a =
        let nm = showName a
            tp = showType a
            anns = []
        in [ MemberDecl 
                (FieldDecl
                   (anns ++ 
                   [ Annotation
                       (NormalAnnotation
                        { annName = Name [Ident "Column"]
                        , annKV =
                            [ ( Ident "name"
                              , EVVal (InitExp (Lit (String $ upperCase nm))))
                            ]
                        })
                   ])
                   (RefType (ClassRefType (ClassType [(Ident tp, [])])))
                   [VarDecl (VarId (Ident $ lowerCase nm)) Nothing])
            , MemberDecl
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

