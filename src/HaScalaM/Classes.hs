{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module HaScalaM.Classes where

import Data.Int (Int8, Int16, Int64)
import HaScalaM.Classes.Base
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type


-- A ---------------------------------------------------------------------------

class ( Init m n t' t ac i
      , Tree a
      ) => Annot m n t' t ac i a
    where init :: a -> i

-- C ---------------------------------------------------------------------------

class ( WithParamClauses m n p t' t pc c
      , Ctor c
      ) => Primary m n p t' t pc c
    where mods :: c -> [m]

class ( WithParamClauses m n p t' t pc c
      , Init m n t' t ac i
      , Stat s
      , Ctor c
      ) => Secondary m n p t' t pc ac i s c

-- I ---------------------------------------------------------------------------

class ( Name n
      , Type' t'
      , ArgClauseT m t ac
      , Tree i
      ) => Init m n t' t ac i
    where tpe        :: i -> t'
          name'      :: i -> n
          argClauses :: i -> [ac]

-- L ---------------------------------------------------------------------------

class Tree t => Lit t

-- P ---------------------------------------------------------------------------

class ( ParamClauseT' m n p' t' b' pc'
      , ParamClauseT m n p t' t pc
      , Tree g
      ) => ParamClauseGroup m n p p' t' b' t pc pc' g
    where
      t'paramClause' :: g -> pc'
      paramClauses' :: g -> [pc]

-- S ---------------------------------------------------------------------------

class ( Name n
      , WithDeclTpeOpt t' p
      ) => Self n t' p

class (Stat s, Tree t) => Source s t
    where stats' :: t -> [s]

-- T ---------------------------------------------------------------------------

class ( Init m n t' t ac i
      , Stat s
      , Self n t' p
      , Tree e
      ) => Template m n t' t ac i p s e
    where early :: e -> [s]
          inits :: e -> [i]
          self  :: e -> p
          stats :: e -> [s]
          derives :: e -> [t']

-- W ---------------------------------------------------------------------------

class ( Primary m n p t' t pc c
      , Tree w
      ) => WithCtor m n p t' t pc c w
    where ctor :: w -> c

class ( Template m n t' t ac i p s e
      , Tree w
      ) => WithTemplate m n t' t ac i p s e w
    where templ :: w -> e

class ( ParamClauseGroup m n p p' t' b' t pc pc' g
      , WithParamClauses m n p t' t pc w
      ) => WithParamClauseGroup m n p p' t' b' t pc pc' g w
    where paramClauseGroup :: w -> Maybe g

class ( ParamClauseGroup m n p p' t' b' t pc pc' g
      , Tree w
      ) => WithParamClauseGroups m n p p' t' b' t pc pc' g w
   where paramClauseGroups :: w -> [g]
