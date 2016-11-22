module HList.HMap where
import HList.HFoldr
import HList.HApply

class HList l => HMap f l r | f l -> r where
    hMap:: f -> l -> r

instance HMap f HNil v where
    hMap _ _ = HNil

instance (HFoldr f v l r, HApply f (e,r) r')
       => HFoldr f v (HCons e l) r' where
    hFoldr f v (HCons e l) = hApply f (e, hFoldr f v l)