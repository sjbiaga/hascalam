module HaScalaM.Types.Enums where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Term


-- E ---------------------------------------------------------------------------

data SmCaseGeneratorE p b where
    SmCaseGeneratorE :: ( Pat p
                        , Term b
                        ) => { patCGE :: p
                             , rhsCGE :: b } -> SmCaseGeneratorE p b

data SmGeneratorE p b where
    SmGeneratorE :: ( Pat p
                    , Term b
                    ) => { patGE :: p
                         , rhsGE :: b } -> SmGeneratorE p b

data SmGuardE b where
    SmGuardE :: Term b => { condGE :: b } -> SmGuardE b

data SmValE p b where
    SmValE :: ( Pat p
              , Term b
              ) => { patVE :: p
                   , rhsVE :: b } -> SmValE p b
