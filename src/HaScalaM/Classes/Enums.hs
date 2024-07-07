{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Classes.Enums where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Pat
import HaScalaM.Classes.Term


--------------------------------------------------------------------------- E --

class Tree t => Enumerator t

class ( Enumerator e
      , Pat p
      , WithBody b e
      ) => Assign p b e
    where pat :: e -> p
          rhs :: e -> b

class Assign p b t => CaseGenerator p b t

class Assign p b t => Generator p b t

class ( Tree t
      , Term b
      ) => Guard b t
    where cond :: t -> b

class Assign p b t => Val p b t

--------------------------------------------------------------------------- W --

class ( Enumerator e
      , WithExprs e w
      ) => WithEnums e w
    where enums :: w -> [e]
          enums = exprs
