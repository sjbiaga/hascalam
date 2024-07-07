{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module HaScalaM.Classes.Type where

import HaScalaM.Classes.Base


--------------------------------------------------------------------------- C --

class ( Type' t'
      , CaseTree t' t' ct
      ) => Type'Case t' ct

--------------------------------------------------------------------------- F --

data FuncParamClause' t' where
    FuncParamClause' :: Type' t' => { valuesFPC' :: [t'] } -> FuncParamClause' t'

class ( Type' t'
      , Function t' (FuncParamClause' t') t' f
      ) => FunctionT' t' f
    where res :: f -> t'

--------------------------------------------------------------------------- M --

class (NameT' n', Member n' t') => MemberT' n' t'

--------------------------------------------------------------------------- N --

class Name n' => NameT' n'

--------------------------------------------------------------------------- P --

class ( WithT'ParamClause m n p' t' b' pc' p'
      , Bounds' t' b'
      ) => ParamT' m n p' t' b' pc'
    where tbounds' :: p' -> b'
          vbounds' :: p' -> [t']
          cbounds' :: p' -> [t']

class ( ParamT' m n p' t' b' pc'
      , ParamClause m n p' pc'
      ) => ParamClauseT' m n p' t' b' pc'

--------------------------------------------------------------------------- T --

class ( Type' t'
      , ArgClause t' t
      ) => ArgClauseT' t' t

class Tree t' => Type' t'

class (Type' t', Tree b') => Bounds' t' b'
    where lo :: b' -> Maybe t'
          hi :: b' -> Maybe t'

--------------------------------------------------------------------------- W --

class ( Type' t'
      , Tree w
      ) => WithDeclTpe t' w
    where decltpe :: w -> t'

class ( Type' t'
      , Tree w
      ) => WithDeclTpeOpt t' w
    where decltpe' :: w -> Maybe t'

class ( ParamClauseT' m n p' t' b' pc'
      , Tree w
      ) => WithT'ParamClause m n p' t' b' pc' w
    where t'paramClause :: w -> pc'
