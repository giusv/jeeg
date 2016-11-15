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

data Environment = Environment { getEnv :: String
                               }
                               
type Generator = ReaderT Environment Identity
runGenerator :: Generator CompilationUnit -> Environment -> CompilationUnit
runGenerator c env = runIdentity (runReaderT c env)


entity :: (HShow pk, HShow fs) => (pk,fs) -> Generator CompilationUnit
entity (pk,fs) = do 
    pdecl <- return $ PackageDecl (Name [Ident "com",Ident "example"])
    idecl <- return $ [ImportDecl False (Name [Ident "java",Ident "util",Ident "ArrayList"]) False]
    tdecl <- return $ [ClassTypeDecl $ ClassDecl [Public,Annotation MarkerAnnotation (Ident "Entity")] (Ident "People") ]
    return $ CompilationUnit (Just pdecl) idecl tdecl



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


