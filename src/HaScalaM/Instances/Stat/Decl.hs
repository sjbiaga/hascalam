{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Stat.Decl where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Pat
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Classes
import HaScalaM.Types.Stat
import HaScalaM.Types.Ref
import HaScalaM.Types.Term
import HaScalaM.Types.Type
import HaScalaM.Types.Tilde
import Data.Maybe (listToMaybe)


--------------------------------------------------------------------------- D --

instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Tree (SmDef'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Decl (SmDef'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Member tn (SmDef'S m n tn p p' t' b' t pc pc' g)
    where name (SmDef'S _ n _ _) = n
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => MemberT tn (SmDef'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithMods m (SmDef'S m n tn p p' t' b' t pc pc' g)
    where mods (SmDef'S ms _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauses m n p t' t pc (SmDef'S m n tn p p' t' b' t pc pc' g)
    where paramClauses (SmDef'S _ _ gs _) = case listToMaybe gs
                                            of Just (SmParamClauseGroup _ pcs) -> pcs
                                               Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmDef'S m n tn p p' t' b' t pc pc' g)
    where paramClauseGroup (SmDef'S _ _ gs _) = listToMaybe gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroups m n p p' t' b' t pc pc' g (SmDef'S m n tn p p' t' b' t pc pc' g)
    where paramClauseGroups (SmDef'S _ _ gs _) = gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithDeclTpe t' (SmDef'S m n tn p p' t' b' t pc pc' g)
    where decltpe (SmDef'S _ _ _ dt) = dt

--------------------------------------------------------------------------- G --

instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Tree (SmGiven'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Decl (SmGiven'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Member tn (SmGiven'S m n tn p p' t' b' t pc pc' g)
    where name (SmGiven'S _ n _ _) = n
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => MemberT tn (SmGiven'S m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithMods m (SmGiven'S m n tn p p' t' b' t pc pc' g)
    where mods (SmGiven'S ms _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauses m n p t' t pc (SmGiven'S m n tn p p' t' b' t pc pc' g)
    where paramClauses (SmGiven'S _ _ pcg _) = case pcg
                                               of Just (SmParamClauseGroup _ pcs) -> pcs
                                                  Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmGiven'S m n tn p p' t' b' t pc pc' g)
    where paramClauseGroup (SmGiven'S _ _ pcg _) = pcg
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithDeclTpe t' (SmGiven'S m n tn p p' t' b' t pc pc' g)
    where decltpe (SmGiven'S _ _ _ dt) = dt

--------------------------------------------------------------------------- T --

instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Tree (SmType'S m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Decl (SmType'S m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithT'ParamClause m n p' t' b' pc' (SmType'S m n t'n p' t' b' pc')
    where t'paramClause (SmType'S _ _ t'pc _) = t'pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithMods m (SmType'S m n t'n p' t' b' pc')
    where mods (SmType'S ms _ _ _) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Member t'n (SmType'S m n t'n p' t' b' pc')
    where name (SmType'S _ n _ _) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => MemberT' t'n (SmType'S m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Type'Def m n t'n p' t' b' pc' (SmType'S m n t'n p' t' b' pc')
    where bounds' (SmType'S _ _ _ b's) = b's

--------------------------------------------------------------------------- V --

instance ( Mod m
         , Pat p
         , Type' t'
         ) => Tree (SmVal'S m p t')
instance ( Mod m
         , Pat p
         , Type' t'
         ) => Decl (SmVal'S m p t')
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithMods m (SmVal'S m p t')
    where mods (SmVal'S ms _ _) = ms
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithPats p (SmVal'S m p t')
    where pats (SmVal'S _ ps _) = ps
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithDeclTpe t' (SmVal'S m p t')
    where decltpe (SmVal'S _ _ dt) = dt

instance ( Mod m
         , Pat p
         , Type' t'
         ) => Tree (SmVar'S m p t')
instance ( Mod m
         , Pat p
         , Type' t'
         ) => Decl (SmVar'S m p t')
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithMods m (SmVar'S m p t')
    where mods (SmVar'S ms _ _) = ms
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithPats p (SmVar'S m p t')
    where pats (SmVar'S _ ps _) = ps
instance ( Mod m
         , Pat p
         , Type' t'
         ) => WithDeclTpe t' (SmVar'S m p t')
    where decltpe (SmVar'S _ _ dt) = dt
