module HaScalaM.Classes.Pat where

import HaScalaM.Classes.Base


--------------------------------------------------------------------------- P --

class Tree t => Pat t

class ( Pat p
      , ArgClause p t
      , WithPats p t
      ) => ArgClauseP p t

--------------------------------------------------------------------------- W --

class ( Pat p
      , Tree w
      ) => WithPats p w
    where pats :: w -> [p]
