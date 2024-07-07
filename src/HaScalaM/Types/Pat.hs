{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Types.Pat where

import HaScalaM.Classes
import HaScalaM.Classes.Pat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Ref


--------------------------------------------------------------------------- P --

data SmExtractP t p ac where
    SmExtractP :: ( Term t
                  , Pat p
                  , ArgClauseP p ac
                  ) => { funXP :: t
                       , argClauseXP :: ac } -> SmExtractP t p ac

data SmExtractInfixP tn p ac where
    SmExtractInfixP :: ( NameT tn
                       , Pat p
                       , ArgClauseP p ac
                       ) => { lhsXxP :: p
                            , opXxP :: tn
                            , argClauseXxP :: ac } -> SmExtractInfixP tn p ac

data SmMacroP t where
    SmMacroP :: Term t => { bodyMP :: t } -> SmMacroP t

data SmTupleP p where
    SmTupleP :: Pat p => { argsTP :: [p] } -> SmTupleP p

data SmVarP tn where
    SmVarP :: NameT tn => { varVP :: tn } -> SmVarP tn
