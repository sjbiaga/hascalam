{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Classes.Enums where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Term


-- E ---------------------------------------------------------------------------

class Tree t => Enumerator t

class ( Pat p
      , Term b
      , WithBody b t
      ) => CaseGenerator p b t
    where patCG :: t -> p
          rhsCG :: t -> b

class ( Pat p
      , Term b
      , WithBody b t
      ) => Generator p b t
    where patG :: t -> p
          rhsG :: t -> b

class ( Tree t
      , Term b
      ) => Guard b t
    where condG :: t -> b

class ( Pat p
      , Term b
      , WithBody b t
      ) => Val p b t
    where patV :: t -> p
          rhsV :: t -> b

-- W ---------------------------------------------------------------------------

class ( Enumerator e
      , Tree w
      ) => WithEnums e w
    where enums :: w -> [e]
