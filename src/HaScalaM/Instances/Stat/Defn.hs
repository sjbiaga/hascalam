{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Stat.Defn where

import Data.Maybe         (listToMaybe)
import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Pat
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Stat
import HaScalaM.Types.Tilde


--------------------------------------------------------------------------- C --

instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Tree (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Defn (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Member t'n (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where name (SmClassS _ n _ _ _) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => MemberT' t'n (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithMods m (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where mods (SmClassS ms _ _ _ _) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithT'ParamClause m n p' t' b' pc' (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where t'paramClause (SmClassS _ _ t'pc _ _) = t'pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithCtor m n p t' t pc c (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where ctor (SmClassS _ _ _ c _) = c
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithTemplate m n t' t ac i f s e (SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where templ (SmClassS _ _ _ _ e) = e

--------------------------------------------------------------------------- D --

instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Tree (SmDefS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Defn (SmDefS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Member tn (SmDefS m n tn p p' t' b' t pc pc' g)
    where name (SmDefS _ n _ _ _) = n
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => MemberT tn (SmDefS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithMods m (SmDefS m n tn p p' t' b' t pc pc' g)
    where mods (SmDefS ms _ _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauses m n p t' t pc (SmDefS m n tn p p' t' b' t pc pc' g)
    where paramClauses (SmDefS _ _ gs _ _) = case listToMaybe gs
                                             of Just (SmParamClauseGroup _ pcs) -> pcs
                                                Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmDefS m n tn p p' t' b' t pc pc' g)
    where paramClauseGroup (SmDefS _ _ gs _ _) = listToMaybe gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroups m n p p' t' b' t pc pc' g (SmDefS m n tn p p' t' b' t pc pc' g)
    where paramClauseGroups (SmDefS _ _ gs _ _) = gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithDeclTpeOpt t' (SmDefS m n tn p p' t' b' t pc pc' g)
    where decltpe' (SmDefS _ _ _ dt _) = dt
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithBody t (SmDefS m n tn p p' t' b' t pc pc' g)
    where body (SmDefS _ _ _ _ b) = b

--------------------------------------------------------------------------- E --

instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Tree (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Defn (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Member t'n (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where name (SmEnumS _ n _ _ _) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => MemberT' t'n (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithMods m (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where mods (SmEnumS ms _ _ _ _) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithT'ParamClause m n p' t' b' pc' (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where t'paramClause (SmEnumS _ _ t'pc _ _) = t'pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithCtor m n p t' t pc c (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where ctor (SmEnumS _ _ _ c _) = c
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithTemplate m n t' t ac i f s e (SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where templ (SmEnumS _ _ _ _ e) = e


instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => Tree (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => Defn (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => Member tn (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
    where name (SmEnumCaseS _ n _ _ _) = n
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => MemberT tn (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => WithMods m (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
    where mods (SmEnumCaseS ms _ _ _ _) = ms
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => WithT'ParamClause m n p' t' b' pc' (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
    where t'paramClause (SmEnumCaseS _ _ t'pc _ _) = t'pc
instance ( NameT tn
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Init m n t' t ac i
         ) => WithCtor m n p t' t pc c (SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i)
    where ctor (SmEnumCaseS _ _ _ c _) = c


instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Stat s
         ) => Tree (SmExtensionGroupS m n p p' t' b' t pc pc' s g)
instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Stat s
         ) => Defn (SmExtensionGroupS m n p p' t' b' t pc pc' s g)
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         , Stat s
         ) => WithParamClauses m n p t' t pc (SmExtensionGroupS m n p p' t' b' t pc pc' s g)
    where paramClauses (SmExtensionGroupS pcg _) = case pcg
                                                   of Just (SmParamClauseGroup _ pcs) -> pcs
                                                      Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         , Stat s
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmExtensionGroupS m n p p' t' b' t pc pc' s g)
    where paramClauseGroup (SmExtensionGroupS pcg _) = pcg
instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Stat s
         ) => WithBody s (SmExtensionGroupS m n p p' t' b' t pc pc' s g)
    where body (SmExtensionGroupS _ b) = b

--------------------------------------------------------------------------- G --

instance ParamClauseGroup m n p p' t' b' t pc pc' g => Tree (SmGivenAliasS m n p p' t' b' t pc pc' g)
instance ParamClauseGroup m n p p' t' b' t pc pc' g => Defn (SmGivenAliasS m n p p' t' b' t pc pc' g)
instance ParamClauseGroup m n p p' t' b' t pc pc' g => WithMods m (SmGivenAliasS m n p p' t' b' t pc pc' g)
    where mods (SmGivenAliasS ms _ _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauses m n p t' t pc (SmGivenAliasS m n p p' t' b' t pc pc' g)
    where paramClauses (SmGivenAliasS _ _ pcg _ _) = case pcg
                                                     of Just (SmParamClauseGroup _ pcs) -> pcs
                                                        Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmGivenAliasS m n p p' t' b' t pc pc' g)
    where paramClauseGroup (SmGivenAliasS _ _ pcg _ _) = pcg
instance ParamClauseGroup m n p p' t' b' t pc pc' g => WithDeclTpe t' (SmGivenAliasS m n p p' t' b' t pc pc' g)
    where decltpe (SmGivenAliasS _ _ _ dt _) = dt
instance ParamClauseGroup m n p p' t' b' t pc pc' g => WithBody t (SmGivenAliasS m n p p' t' b' t pc pc' g)
    where body (SmGivenAliasS _ _ _ _ b) = b


instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => Tree (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => Defn (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => WithMods m (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
    where mods (SmGivenS ms _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => WithParamClauses m n p t' t pc (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
    where paramClauses (SmGivenS _ _ pcg _) = case pcg
                                              of Just (SmParamClauseGroup _ pcs) -> pcs
                                                 Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
    where paramClauseGroup (SmGivenS _ _ pcg _) = pcg
instance ( ParamClauseGroup m n p p' t' b' t pc pc' g
         , Template m n t' t ac i f s e
         ) => WithTemplate m n t' t ac i f s e (SmGivenS m n p p' t' b' t pc pc' ac i f s e g)
    where templ (SmGivenS _ _ _ e) = e

--------------------------------------------------------------------------- M --

instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Tree (SmMacroS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Defn (SmMacroS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => Member tn (SmMacroS m n tn p p' t' b' t pc pc' g)
    where name (SmMacroS _ n _ _ _) = n
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => MemberT tn (SmMacroS m n tn p p' t' b' t pc pc' g)
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithMods m (SmMacroS m n tn p p' t' b' t pc pc' g)
    where mods (SmMacroS ms _ _ _ _) = ms
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
         ) => WithParamClauses m n p t' t pc (SmMacroS m n tn p p' t' b' t pc pc' g)
    where paramClauses (SmMacroS _ _ gs _ _) = case listToMaybe gs
                                               of Just (SmParamClauseGroup _ pcs) -> pcs
                                                  Nothing                         -> []
instance ( g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
         , NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
      ) => WithParamClauseGroup m n p p' t' b' t pc pc' g (SmMacroS m n tn p p' t' b' t pc pc' g)
    where paramClauseGroup (SmMacroS _ _ gs _ _) = listToMaybe gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
      ) => WithParamClauseGroups m n p p' t' b' t pc pc' g (SmMacroS m n tn p p' t' b' t pc pc' g)
    where paramClauseGroups (SmMacroS _ _ gs _ _) = gs
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
      ) => WithDeclTpeOpt t' (SmMacroS m n tn p p' t' b' t pc pc' g)
    where decltpe' (SmMacroS _ _ _ dt _) = dt
instance ( NameT tn
         , ParamClauseGroup m n p p' t' b' t pc pc' g
      ) => WithBody t (SmMacroS m n tn p p' t' b' t pc pc' g)
    where body (SmMacroS _ _ _ _ b) = b

--------------------------------------------------------------------------- O --

instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => Tree (SmObjectS m n tn p t' t ac i s e)
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => Defn (SmObjectS m n tn p t' t ac i s e)
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => Member tn (SmObjectS m n tn p t' t ac i s e)
    where name (SmObjectS _ n _) = n
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => MemberT tn (SmObjectS m n tn p t' t ac i s e)
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => WithMods m (SmObjectS m n tn p t' t ac i s e)
    where mods (SmObjectS ms _ _) = ms
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => WithTemplate m n t' t ac i p s e (SmObjectS m n tn p t' t ac i s e)
    where templ (SmObjectS _ _ e) = e

--------------------------------------------------------------------------- R --

instance ( Mod m
         , NameT tn
         ) => Tree (SmRepeatedEnumCase m tn)
instance ( Mod m
         , NameT tn
         ) => Defn (SmRepeatedEnumCase m tn)
instance ( Mod m
         , NameT tn
         ) => WithMods m (SmRepeatedEnumCase m tn)
    where mods (SmRepeatedEnumCase ms _) = ms

--------------------------------------------------------------------------- T --

instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Tree (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Defn (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => Member t'n (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where name (SmTraitS _ n _ _ _) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => MemberT' t'n (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithMods m (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where mods (SmTraitS ms _ _ _ _) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithT'ParamClause m n p' t' b' pc' (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where t'paramClause (SmTraitS _ _ t'pc _ _) = t'pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithCtor m n p t' t pc c (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where ctor (SmTraitS _ _ _ c _) = c
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         , Primary m n p t' t pc c
         , Template m n t' t ac i f s e
         ) => WithTemplate m n t' t ac i f s e (SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e)
    where templ (SmTraitS _ _ _ _ e) = e


instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Tree (SmTypeS m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Decl (SmTypeS m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithT'ParamClause m n p' t' b' pc' (SmTypeS m n t'n p' t' b' pc')
    where t'paramClause (SmTypeS _ _ t'pc _ _) = t'pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithMods m (SmTypeS m n t'n p' t' b' pc')
    where mods (SmTypeS ms _ _ _ _) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Member t'n (SmTypeS m n t'n p' t' b' pc')
    where name (SmTypeS _ n _ _ _) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => MemberT' t'n (SmTypeS m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Type'Def m n t'n p' t' b' pc' (SmTypeS m n t'n p' t' b' pc')
    where bounds' (SmTypeS _ _ _ _ b's) = b's
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithBody t' (SmTypeS m n t'n p' t' b' pc')
    where body (SmTypeS _ _ _ b' _) = b'

--------------------------------------------------------------------------- V --

instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => Tree (SmValS m p t' t)
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => Defn (SmValS m p t' t)
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithMods m (SmValS m p t' t)
    where mods (SmValS ms _ _ _) = ms
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithPats p (SmValS m p t' t)
    where pats (SmValS _ ps _ _) = ps
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithDeclTpeOpt t' (SmValS m p t' t)
    where decltpe' (SmValS _ _ dt _) = dt
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithBody t (SmValS m p t' t)
    where body (SmValS _ _ _ b) = b


instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => Tree (SmVarS m p t' t)
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => Defn (SmVarS m p t' t)
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithMods m (SmVarS m p t' t)
    where mods (SmVarS ms _ _ _) = ms
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithPats p (SmVarS m p t' t)
    where pats (SmVarS _ ps _ _) = ps
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithDeclTpeOpt t' (SmVarS m p t' t)
    where decltpe' (SmVarS _ _ dt _) = dt
instance ( Mod m
         , Pat p
         , Type' t'
         , Term t
         ) => WithBody t (SmVarS m p t' t)
    where body (SmVarS _ _ _ b) = b
