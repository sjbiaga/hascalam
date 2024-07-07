module HaScalaM.Types.Ref where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type


-------------------------------------------------------------------------- RT --

data SmRefT n t where
    SmApplyUnaryRT :: ( Name n
                      , Term t
                      ) => { opAURT :: n
                           , argAURT :: t } -> SmRefT n t
    SmSuperRT :: Name n => { thispSRT :: n
                           , superpSRT :: n } -> SmRefT n t
    SmThisRT :: Name n => { nameThRT :: n } -> SmRefT n t

data SmAnonymousRT where
    SmAnonymousRT :: SmAnonymousRT

data SmSelectRT tn t where
    SmSelectRT :: ( Name tn
                  , Term t
                  ) => { qualSRT :: t
                       , nameSRT :: tn } -> SmSelectRT tn t

------------------------------------------------------------------------- RT' --

data SmRefT' t'n t' r where
    SmProjectRT' :: ( NameT' t'n
                    , Type' t'
                    ) => { qualPRT' :: t'
                         , namePRT' :: t'n } -> SmRefT' t'n t' r
    SmSelectRT' :: ( NameT' t'n
                   , RefT r
                   ) => { qualSRT' :: r
                        , nameSRT' :: t'n } -> SmRefT' t'n t' r
    SmSingletonRT' :: RefT r => { refSRT' :: r } -> SmRefT' t'n t' r
