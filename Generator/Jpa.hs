module Generator.Jpa where

import Generator.Commons
import Language.Java.Syntax
import Language.Java.Pretty
-- import Data.Text
import Language.Entity
import Text.PrettyPrint
import Text.Printf
import System.FilePath
import Data.Char
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Debug.Trace

import HList.HShow
import HList.HFoldr
data Environment = Environment { getEnv :: String
                               }
                               
type Generator = ReaderT Environment Identity
runGenerator :: Generator CompilationUnit -> Environment -> CompilationUnit
runGenerator c env = runIdentity (runReaderT c env)


entity :: (HShow pk, HShow fk, Show e) => (e,(pk,fk)) -> Generator CompilationUnit
entity (e,(pk,fk)) = do 
    -- members <- sequence $ map member (pk ++ fk)
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
             (ClassBody
                [ MemberDecl
                    (FieldDecl
                       [ Annotation
                           (NormalAnnotation
                            { annName = Name [Ident "Column"]
                            , annKV =
                                [ ( Ident "name"
                                  , EVVal (InitExp (Lit (String "USER_ID"))))
                                ]
                            })
                       ]
                       (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                       [VarDecl (VarId (Ident "userId")) Nothing])
                , MemberDecl
                    (FieldDecl
                       [ Annotation
                           (NormalAnnotation
                            { annName = Name [Ident "Column"]
                            , annKV =
                                [ ( Ident "name"
                                  , EVVal (InitExp (Lit (String "USER_NAME"))))
                                ]
                            })
                       ]
                       (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                       [VarDecl (VarId (Ident "username")) Nothing])
                , MemberDecl
                    (FieldDecl
                       []
                       (RefType (ClassRefType (ClassType [(Ident "String", [])])))
                       [VarDecl (VarId (Ident "email")) Nothing])
                , MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       (Just
                          (RefType (ClassRefType (ClassType [(Ident "String", [])]))))
                       (Ident "getEmail")
                       []
                       []
                       (MethodBody
                          (Just
                             (Block
                                [ BlockStmt
                                    (Return (Just (ExpName (Name [Ident "email"]))))
                                ]))))
                , MemberDecl
                    (MethodDecl
                       [Public]
                       []
                       Nothing
                       (Ident "setEmail")
                       [ FormalParam
                           []
                           (RefType
                              (ClassRefType (ClassType [(Ident "String", [])])))
                           False
                           (VarId (Ident "email"))
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
                                                (Ident "email")))
                                          EqualA
                                          (ExpName (Name [Ident "email"]))))
                                ]))))
                ]))
      ]



-- generate :: ER -> [OutputFile]
-- generate er = map (uncurry entity) (entities er)


-- entity :: String -> Entity -> OutputFile
-- entity table (Entity name attrs) = OutputFile fileName doc 
    -- where fileName = addExtension ("it" </> "bancaditalia" </> "entity" </> capName) "java"
          -- doc = text "package it.bancaditalia.entities." <> text capName <> semi
             -- $$ text ""
             -- $$ text "@Entity"
             -- $$ text "@Table(name="<> text (escape capTable) <> text ")"
             -- $$ text "public class" <+> text capName <+> text "{"

             -- $$ text "}"
          -- capName = capitalize name
          -- capTable = capitalize table


