module Language.Expression where

data Expression a b = Expression b deriving (Eq,Show)

data IdExpr = IdExpr String deriving (Eq,Show)
fromIdentifier  :: String  -> Expression String IdExpr
fromIdentifier m = Expression (IdExpr m)

data AttrExpr = AttrExpr String deriving (Eq,Show)
fromAttribute :: String -> Expression String AttrExpr
fromAttribute attr = Expression (AttrExpr attr)

data And e1 e2 = And e1 e2 deriving (Eq,Show)
(*&&*):: Expression Bool e1 -> Expression Bool e2 -> Expression Bool (And e1 e2)
(*&&*) (Expression e1) (Expression e2) = Expression (And e1 e2)

data Equals e1 e2 = Equals e1 e2 deriving (Eq,Show)
(*==*) :: Eq a => Expression a e  -> Expression a e -> Expression Bool (Equals e e)
(*==*) (Expression e1) (Expression e2) = Expression (Equals e1 e2)
    
    
                    
                    