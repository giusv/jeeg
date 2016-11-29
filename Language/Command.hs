module Language.Command where

data Variable = Variable String deriving (Eq,Show)
type Response = (Variable,Variable)
data Skip = Skip
data Get = Get e Response
data PrimCommand = Skip
                 | HttpGet Expr Response
                 | HttpPost Expr Payload Response
                 | HttpPatch Expr Payload Response
                 | HttpPut Expr Payload Response
                 | DBQuery Name Sql Variable
                 | Assign Expr Variable
                 | OrElse PrimCommand PrimCommand
                 | Foreach Expr PrimCommand Variable
                 | WithKeystore String String PrimCommand
                 | WithNamespace [Namespace] PrimCommand
                 | WithTimeout Int PrimCommand Variable
                 | WithConnections [Connection] PrimCommand
                 -- | WithProfiling [String] Int PrimCommand
                 | Wait Expr
                 | Assert Expr
                 | Choose Expr PrimCommand PrimCommand
                 | Concatenate PrimCommand PrimCommand
                 | Navigate Navigation
                 | FindElements By Element ElementList
                 | FindElement By Element Element
                 | GetAttribute Name Element Attribute
                 | GetProperty Name Element Property
                 | Click Element
                 | Send String Element
                 | Frame String
                 | Window String
                 | Alert
                 
                 