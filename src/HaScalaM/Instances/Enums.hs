module HaScalaM.Instances.Enums where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Enums
import HaScalaM.Classes.Pat
import HaScalaM.Classes.Term
import HaScalaM.Types.Enums
import HaScalaM.Types.Tilde (SmEnumerator)


--------------------------------------------------------------------------- E --

instance ( Pat p
         , Term b
         ) => Tree (SmCaseGeneratorE p b)
instance ( Pat p
         , Term b
         ) => Enumerator (SmCaseGeneratorE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmCaseGeneratorE p b)
    where body (SmCaseGeneratorE _ rhs) = rhs
instance ( Pat p
         , Term b
         ) => Assign p b (SmCaseGeneratorE p b)
    where pat (SmCaseGeneratorE p _) = p
          rhs (SmCaseGeneratorE _ r) = r
instance ( Pat p
         , Term b
         ) => CaseGenerator p b (SmCaseGeneratorE p b)

instance ( Pat p
         , Term b
         ) => Tree (SmGeneratorE p b)
instance ( Pat p
         , Term b
         ) => Enumerator (SmGeneratorE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmGeneratorE p b)
    where body (SmGeneratorE _ rhs) = rhs
instance ( Pat p
         , Term b
         ) => Assign p b (SmGeneratorE p b)
    where pat (SmGeneratorE p _) = p
          rhs (SmGeneratorE _ r) = r
instance ( Pat p
         , Term b
         ) => Generator p b (SmGeneratorE p b)

instance Term c => Tree (SmGuardE c)
instance Term c => WithCond c (SmGuardE c)
    where cond (SmGuardE c) = c
instance Term c => Guard c (SmGuardE c)
    where cond (SmGuardE c) = c

instance ( Pat p
         , Term b
         ) => Tree (SmValE p b)
instance ( Pat p
         , Term b
         ) => Enumerator (SmValE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmValE p b)
    where body (SmValE _ rhs) = rhs
instance ( Pat p
         , Term b
         ) => Assign p b (SmValE p b)
    where pat (SmValE p _) = p
          rhs (SmValE _ r) = r
instance ( Pat p
         , Term b
         ) => Val p b (SmValE p b)

instance Tree SmEnumerator
instance Enumerator SmEnumerator
