{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Tilde where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Tilde


-- C ---------------------------------------------------------------------------

instance ParamClauseT m n p t' t pc => Tree (SmCtorPrimary m n p t' t pc)
instance ParamClauseT m n p t' t pc => Ctor (SmCtorPrimary m n p t' t pc)
instance ParamClauseT m n p t' t pc => WithParamClauses m n p t' t pc (SmCtorPrimary m n p t' t pc)
    where paramClauses (SmCtorPrimary _ _ pcs) = pcs
instance ( m ~ SmMod
         , n ~ SmName
         , p ~ SmParamT m n t' t
         , t' ~ SmType'
         , t ~ SmTerm
         , pc ~ SmParamClauseT m n p t' t
         , ParamClauseT m n p t' t pc
         ) => Primary m n p t' t pc (SmCtorPrimary m n p t' t pc)
    where mods (SmCtorPrimary ms _ _) = ms

-- M ---------------------------------------------------------------------------

instance Tree SmMod
instance Mod SmMod
instance ArgsType SmMod
instance ParamsType SmMod
instance Variant SmMod

-- P ---------------------------------------------------------------------------

instance ( ParamClauseT' m n p' t' b' pc'
         , ParamClauseT m n p t' t pc
         ) => Tree (SmParamClauseGroup m n p p' t' b' t pc pc')
instance ( m ~ SmMod
         , n ~ SmName
         , p ~ SmParamT m n t' t
         , p' ~ SmParamT' m n t' b'
         , t' ~ SmType'
         , b' ~ SmBounds' t'
         , t ~ SmTerm
         , pc ~ SmParamClauseT m n p t' t
         , pc' ~ SmParamClauseT' m n p' t' b'
         , ParamClauseT' m n p' t' b' pc'
         , ParamClauseT m n p t' t pc
         ) => ParamClauseGroup m n p p' t' b' t pc pc' (SmParamClauseGroup m n p p' t' b' t pc pc')
    where
      t'paramClause' (SmParamClauseGroup t'pc _) = t'pc
      paramClauses' (SmParamClauseGroup _ pcs) = pcs

-- S ---------------------------------------------------------------------------

instance ( Name n
         , Type' t'
         ) => Tree (SmSelf n t')
instance ( Name n
         , Type' t'
         ) => Member n (SmSelf n t')
    where name (SmSelf n _) = n
instance ( Name n
         , Type' t'
         ) => WithDeclTpeOpt t' (SmSelf n t')
    where decltpeOpt (SmSelf _ dt) = dt
instance ( n ~ SmName
         , t' ~ SmType'
         , Name n
         , Type' t'
         ) => Self n t' (SmSelf n t')


instance Stat s => Tree (SmSource s)
instance ( s ~ SmStat
         , Stat s
         ) => Source s (SmSource s)
    where stats' (SmSource ss) = ss

-- T ---------------------------------------------------------------------------

instance ( Init m n t' t ac i
         , Stat s
         , Self n t' p
         ) => Tree (SmTemplate m n t' t ac i p s)
instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , t ~ SmTerm
         , ac ~ SmArgClauseT m t
         , i ~ SmInit m n t' t ac
         , s ~ SmStat
         , p ~ SmSelf n t'
         , Init m n t' t ac i
         , Stat s
         , Self n t' p
         ) => Template m n t' t ac i p s (SmTemplate m n t' t ac i p s)
    where early (SmTemplate ss _ _ _ _) = ss
          inits (SmTemplate _ is _ _ _) = is
          self  (SmTemplate _ _ s _ _) = s
          stats (SmTemplate _ _ _ ss _) = ss
          derives (SmTemplate _ _ _ _ t's) = t's
