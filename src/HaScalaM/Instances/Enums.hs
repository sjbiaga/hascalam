module HaScalaM.Instances.Enums where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Enums
import HaScalaM.Classes.Term
import HaScalaM.Types.Enums
import HaScalaM.Types.Tilde (SmEnumerator)


-- E ---------------------------------------------------------------------------

instance ( Pat p
         , Term b
         ) => Tree (SmCaseGeneratorE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmCaseGeneratorE p b) where
    body (SmCaseGeneratorE _ rhs) = rhs
instance ( Pat p
         , Term b
         ) => CaseGenerator p b (SmCaseGeneratorE p b)
    where patCG (SmCaseGeneratorE p _) = p
          rhsCG (SmCaseGeneratorE _ r) = r

instance ( Pat p
         , Term b
         ) => Tree (SmGeneratorE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmGeneratorE p b) where
    body (SmGeneratorE _ rhs) = rhs
instance ( Pat p
         , Term b
         , WithBody (SmGeneratorE p b) b
         ) => Generator p b (SmGeneratorE p b)
    where patG (SmGeneratorE p _) = p
          rhsG (SmGeneratorE _ r) = r

instance Term b => Tree (SmGuardE b)
instance Term b => Guard b (SmGuardE b)
    where condG (SmGuardE c) = c

instance ( Pat p
         , Term b
         ) => Tree (SmValE p b)
instance ( Pat p
         , Term b
         ) => WithBody b (SmValE p b) where
    body (SmValE _ rhs) = rhs
instance ( Pat p
         , Term b
         ) => Val p b (SmValE p b)
    where patV (SmValE p _) = p
          rhsV (SmValE _ r) = r

instance Tree SmEnumerator
instance Enumerator SmEnumerator
