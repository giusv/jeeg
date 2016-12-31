module MyHList.HShow where
import MyHList.HBasic

class (HList l) => HShow l where
    hShow :: l -> [String]
    
instance HShow HNil where
    hShow HNil = []
instance (Show e, HShow l) => HShow (HCons e l) where
    hShow (HCons e l) = show e : hShow l
    