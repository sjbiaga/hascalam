{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Classes.Base where


-- C ---------------------------------------------------------------------------

class (Tree t, WithBody b ct) => CaseTree t b ct
    where pat :: ct -> t

class Tree t => Ctor t

-- F ---------------------------------------------------------------------------

class ( SyntaxValuesClauses v pc
      , WithBody t f
      ) => Function v pc t f
    where paramClause :: f -> pc

-- M ---------------------------------------------------------------------------

class (Name n, Tree t) => Member n t
    where name :: t -> n

class (Tree v, Tree t) => SyntaxValuesClauses v t
    where values :: t -> [v]

class SyntaxValuesClauses v t => ArgClause v t

class ( Tree f
      , ArgClause v t
      , Tree y
      ) => Apply f v t y
    where fun :: y -> f
          argClause :: y -> t

class ( Tree l
      , Name n
      , Tree a
      , Tree x
      ) => Infix l n a x
    where lhs :: x -> l
          op  :: x -> n
          arg :: x -> a

class (Tree v, Tree t) => Tuple v t
    where args :: t -> [v]


class Tree m => Mod m

class Mod m => ArgsType m

class Mod m => ParamsType m

class Mod m => Variant m

-- N ---------------------------------------------------------------------------

class Tree n => Name n
    where value :: n -> String

-- P ---------------------------------------------------------------------------

class ( Mod m
      , Name n
      , Member n p
      ) => Param m n p
    where mods' :: p -> [m]

class ( Param m n p
      , SyntaxValuesClauses p pc
      ) => ParamClause m n p pc

class Tree t => Pat t

class (Pat p, ArgClause p t) => ArgClauseP p t

-- R ---------------------------------------------------------------------------

class Tree t => Ref t

-- S ---------------------------------------------------------------------------

class Tree s => Stat s

-- T ---------------------------------------------------------------------------

class Tree t

-- W ---------------------------------------------------------------------------

class ( Tree t
      , Tree w
      ) => WithBody t w
    where body :: w -> t

class ( CaseTree t b ct
      , Tree w
      ) => WithCases t b ct w
    where cases :: w -> [ct]

class ( Mod m
      , Tree w
      ) => WithMods m w
    where mods :: w -> [m]

class ( Ref r
      , Tree w
      ) => WithWithin r w
    where within :: w -> r
