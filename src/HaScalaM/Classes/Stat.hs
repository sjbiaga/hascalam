{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module HaScalaM.Classes.Stat where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Type


-- D ---------------------------------------------------------------------------

class Tree s => Decl s

class Tree s => Defn s

-- I ---------------------------------------------------------------------------

class Tree r => Importee r

class (RefT r, Importee i, Tree t) => Importer r i t
    where ref :: t -> r
          importees :: t -> [i]

class (Importer r i t, Tree s) => ImportExportStat r i t s
    where importers :: s -> [t]

-- S ---------------------------------------------------------------------------

class ( MemberT' t'n t'd
      , WithMods m t'd
      , WithT'ParamClause m n p' t' b' pc' t'd
      ) => Type'Def m n t'n p' t' b' pc' t'd
    where bounds' :: t'd -> b'
