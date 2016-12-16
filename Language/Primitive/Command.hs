module Language.Primitive.Command where
import Language.Primitive.Expression

data Variable = Variable String deriving (Eq,Show)
type Response = (Variable,Variable)
data Skip = Skip
data Get e = Get e Response
data Post e b = Post e b Response
data Concatenate a b = Concatenate a b
