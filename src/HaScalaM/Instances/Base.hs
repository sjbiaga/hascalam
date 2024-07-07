{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Base where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Base
import HaScalaM.Types.Tilde


--------------------------------------------------------------------------- I --

instance ( Name n
         , Type' t'
         , ArgClauseT m t ac
         ) => Tree (SmInit m n t' t ac)
instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , ac ~ SmArgClauseT m t
         , Name n
         , Type' t'
         , ArgClauseT m t ac
         ) => Init m n t' t ac (SmInit m n t' t ac)
    where tpe   (SmInit t _ _) = t
          name' (SmInit _ n _) = n
          argClauses (SmInit _ _ as) = as

--------------------------------------------------------------------------- L --

instance Tree SmLit
instance Lit SmLit

--------------------------------------------------------------------------- M --

instance Tree SmModM

instance Ref r => Tree (SmAccessM r)
instance Ref r => WithWithin r (SmAccessM r)
    where within (SmPrivateM r)   = r
          within (SmProtectedM r) = r
