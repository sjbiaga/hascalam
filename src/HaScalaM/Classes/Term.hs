{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Classes.Term where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Type


-- C ---------------------------------------------------------------------------

class ( Pat p
      , Term t
      , CaseTree p t ct
      ) => Case p t ct

-- F ---------------------------------------------------------------------------

class ( ParamClause m n p pc
      , ParamT m n p t' t
      , Function p pc t f
      ) => FunctionT m n p pc t' t f

-- M ---------------------------------------------------------------------------

class (NameT n, Member n t) => MemberT n t

-- N ---------------------------------------------------------------------------

class Name t => NameT t

-- P ---------------------------------------------------------------------------

class ( Param m n p
      , WithDeclTpeOpt t' p
      , Term t
      ) => ParamT m n p t' t
    where defaultOpt :: p -> Maybe t

class ( ParamsType m
      , ParamT m n p t' t
      , ParamClause m n p pc
      ) => ParamClauseT m n p t' t pc
    where mod' :: pc -> Maybe m

-- T ---------------------------------------------------------------------------

class ( ArgsType m
      , Term t
      , ArgClause t ac
      ) => ArgClauseT m t ac
    where mod :: ac -> Maybe m

class Tree t => Term t

-- W ---------------------------------------------------------------------------

class ( ParamClauseT m n p t' t pc
      , Tree w
      ) => WithParamClauses m n p t' t pc w
    where paramClauses :: w -> [pc]
