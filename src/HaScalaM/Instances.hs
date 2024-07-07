module HaScalaM.Instances where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Base
import HaScalaM.Types.Tilde


--------------------------------------------------------------------------- A --

instance Init m n t' t ac i => Tree (SmAnnotM m n t' t ac i)
instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , t ~ SmTerm
         , ac ~ SmArgClauseT m t
         , i ~ SmInit m n t' t ac
         , Init m n t' t ac i
         ) => Annot m n t' t ac i (SmAnnotM m n t' t ac i)
    where init (SmAnnotM i) = i

--------------------------------------------------------------------------- C --

instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => Tree (SmCtorSecondaryS m n p t' t pc ac i s)
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => Ctor (SmCtorSecondaryS m n p t' t pc ac i s)
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => WithMods m (SmCtorSecondaryS m n p t' t pc ac i s)
    where mods (SmCtorSecondaryS ms _ _ _ _) = ms
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => WithParamClauses m n p t' t pc (SmCtorSecondaryS m n p t' t pc ac i s)
    where paramClauses (SmCtorSecondaryS _ _ pss _ _) = pss
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => WithExprs s (SmCtorSecondaryS m n p t' t pc ac i s)
    where exprs (SmCtorSecondaryS _ _ _ _ ss) = ss
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => WithStats s (SmCtorSecondaryS m n p t' t pc ac i s)
instance ( ParamClauseT m n p t' t pc
         , Init m n t' t ac i
         , Stat s
         ) => Secondary m n p t' t pc ac i s (SmCtorSecondaryS m n p t' t pc ac i s)
