module HaScalaM.Instances.Ref where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Base
import HaScalaM.Types.Ref
import HaScalaM.Types.Tilde


--------------------------------------------------------------------------- N --

instance Tree SmName
instance Name SmName where
    value (NAnonymous SmAnonymousRT)   = ""
    value (NName SmAnonymousN)         = ""
    value (NName (SmIndeterminateN v)) = v
    value (NName SmPlaceholderN)       = "_"
    value (NName SmThisN)              = "this"
    value (NTName n)                   = value n
    value (NT'Name n')                 = value n'


instance Tree SmNameT
instance Name SmNameT
    where value (SmNameT v) = v
instance NameT SmNameT


instance Tree SmNameT'
instance Name SmNameT'
    where value (SmNameT' v) = v
instance NameT' SmNameT'

--------------------------------------------------------------------------- R --

instance Tree SmRef_
instance Ref SmRef_


instance Tree SmRef
instance RefT SmRef


instance Tree SmRef'
instance RefT' SmRef'
