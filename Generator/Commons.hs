module Generator.Commons where
import Text.PrettyPrint
import Text.Printf
import System.FilePath
import System.Directory
import Data.Char
data OutputFile = OutputFile { getFilePath :: FilePath   -- file path
                             , getDoc :: Doc        -- contained document 
                             }
    deriving (Show)
    
saveFile :: FilePath -> OutputFile -> IO ()
saveFile baseDirectory (OutputFile relativeFilePath sourceCode) = 
    let fileName = takeFileName relativeFilePath 
        targetDirectory = baseDirectory </> (takeDirectory relativeFilePath)
        absoluteFileName = targetDirectory </> fileName in
            do { createDirectoryIfMissing True targetDirectory
               ; writeFile absoluteFileName (render sourceCode)
               }
               
capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : (map toLower t)

trycatch :: Doc -> [(String,Doc)] -> Doc
trycatch try xs = 
    text "try {"
 $$ nest 4 try
 $$ text "}"
 $$ vcat (map catch xs)
 where catch (e,c) = text "catch" <> parens (text e) <+> text "{"
                  $$ nest 4 c
                  $$ text "}"
upperCase :: String -> String
upperCase  = map toUpper

lowerCase :: String -> String
lowerCase  = map toLower

postfix c s = text s <> text (show c)

vcatstr :: [Doc] -> Doc
vcatstr docs = vcat $ punctuate (text " + ") (map (\d -> doubleQuotes (d <> text "\\n")) docs)

escape :: String -> String
escape = concatMap escape'
escape' :: Char -> String
escape' '\"' = "\\\""
-- escape' ' ' = ""
escape' c = [c]
