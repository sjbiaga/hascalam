{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Pat where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Term
import HaScalaM.Types.Pat
import HaScalaM.Types.Tilde


-- P ---------------------------------------------------------------------------

instance Pat p => Tree (SmArgClauseP p)
instance Pat p => SyntaxValuesClauses p (SmArgClauseP p)
    where values (SmArgClauseP vs) = vs
instance Pat p => ArgClause p (SmArgClauseP p)
instance Pat p => ArgClauseP p (SmArgClauseP p)

instance Tree SmPatP

instance ( Term t
         , Pat p
         , ArgClauseP p ac
         ) => Tree (SmExtractP t p ac)
instance ( Term t
         , Pat p
         , ArgClauseP p ac
         ) => Apply t p ac (SmExtractP t p ac)
    where fun       (SmExtractP f _) = f
          argClause (SmExtractP _ ac) = ac

instance ( NameT tn
         , Pat p
         , ArgClauseP p ac
         ) => Tree (SmExtractInfixP tn p ac)
instance ( NameT tn
         , Pat p
         , ArgClauseP p ac
         ) => Infix p tn ac (SmExtractInfixP tn p ac)
    where lhs (SmExtractInfixP l _ _) = l
          op  (SmExtractInfixP _ n _) = n
          arg (SmExtractInfixP _ _ ac) = ac

instance Term t => Tree (SmMacroP t)
instance Term t => WithBody t (SmMacroP t)
    where body (SmMacroP b) = b

instance Pat p => Tree (SmTupleP p)
instance Pat p => Tuple p (SmTupleP p)
    where args (SmTupleP as) = as

instance NameT tn => Tree (SmVarP tn)
instance NameT tn => Member tn (SmVarP tn)
    where name (SmVarP n) = n
instance NameT tn => MemberT tn (SmVarP tn)

instance Tree SmPat
instance Pat SmPat
