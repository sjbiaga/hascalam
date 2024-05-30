module HaScalaM.Types.Stat where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type


-- D ---------------------------------------------------------------------------

data SmDef'S m n tn p p' t' b' t pc pc' g where
    SmDef'S :: ( NameT tn
               , ParamClauseGroup m n p p' t' b' t pc pc' g
               ) => { modsD'S :: [m]
                    , nameD'S :: tn
                    , paramClauseGroupsD'S :: [g]
                    , decltpeD'S :: t' } -> SmDef'S m n tn p p' t' b' t pc pc' g

data SmGiven'S m n tn p p' t' b' t pc pc' g where
    SmGiven'S :: ( NameT tn
                 , ParamClauseGroup m n p p' t' b' t pc pc' g
                 ) => { modsG'S :: [m]
                      , nameG'S :: tn
                      , paramClauseGroupOptG'S :: Maybe g
                      , decltpeG'S :: t' } -> SmGiven'S m n tn p p' t' b' t pc pc' g

data SmType'S m n t'n p' t' b' pc' where
    SmType'S :: ( NameT' t'n
                , ParamClauseT' m n p' t' b' pc'
                ) => { modsT'S :: [m]
                     , nameT'S :: t'n
                     , t'paramClauseT'S :: pc'
                     , boundsT'S :: b' } -> SmType'S m n t'n p' t' b' pc'

data SmVal'S m p t' where
    SmVal'S :: ( Mod m
               , Pat p
               , Type' t'
               ) => { modsVal'S :: [m]
                    , patsVal'S :: [p]
                    , decltpeVal'S :: t' } -> SmVal'S m p t'

data SmVar'S m p t' where
    SmVar'S :: ( Mod m
               , Pat p
               , Type' t'
               ) => { modsVar'S :: [m]
                    , patsVar'S :: [p]
                    , decltpeVar'S :: t' } -> SmVar'S m p t'


data SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e where
    SmClassS :: ( NameT' t'n
                , ParamClauseT' m n p' t' b' pc'
                , Primary m n p t' t pc c
                , Template m n t' t ac i f s e
                ) => { modsCS :: [m]
                     , nameCS :: t'n
                     , t'paramClauseCS :: pc'
                     , ctorCS :: c
                     , templCS :: e } -> SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e

data SmDefS m n tn p p' t' b' t pc pc' g where
    SmDefS :: ( NameT tn
              , ParamClauseGroup m n p p' t' b' t pc pc' g
              ) => { modsDS :: [m]
                   , nameDS :: tn
                   , paramClauseGroupsDS :: [g]
                   , decltpeOptDS :: Maybe t'
                   , bodyDS :: t } -> SmDefS m n tn p p' t' b' t pc pc' g

data SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e where
    SmEnumS :: ( NameT' t'n
               , ParamClauseT' m n p' t' b' pc'
               , Primary m n p t' t pc c
               , Template m n t' t ac i f s e
               ) => { modsES :: [m]
                    , nameES :: t'n
                    , t'paramClauseES :: pc'
                    , ctorES :: c
                    , templES :: e } -> SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e

data SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i where
    SmEnumCaseS :: ( NameT tn
                   , ParamClauseT' m n p' t' b' pc'
                   , Primary m n p t' t pc c
                   , Init m n t' t ac i
                   ) => { modsECS :: [m]
                        , nameECS :: tn
                        , t'paramClauseECS :: pc'
                        , ctorECS :: c
                        , initsECS :: [i] } -> SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i

data SmExtensionGroupS m n p p' t' b' t pc pc' s g where
    SmExtensionGroupS :: ( ParamClauseGroup m n p p' t' b' t pc pc' g
                         , Stat s
                         ) => { paramClauseGroupOptEGS :: Maybe g
                              , bodyEGS :: s } -> SmExtensionGroupS m n p p' t' b' t pc pc' s g

data SmGivenS m n p p' t' b' t pc pc' ac i f s e g where
    SmGivenS :: ( ParamClauseGroup m n p p' t' b' t pc pc' g
                , Template m n t' t ac i f s e
                ) => { modsGS :: [m]
                     , nameGS :: n
                     , paramClauseGroupOptGS :: Maybe g
                     , templGS :: e } -> SmGivenS m n p p' t' b' t pc pc' ac i f s e g

data SmGivenAliasS m n p p' t' b' t pc pc' g where
    SmGivenAliasS :: ParamClauseGroup m n p p' t' b' t pc pc' g => { modsGAS :: [m]
                                                                   , nameGAS :: n
                                                                   , paramClauseGroupOptGAS :: Maybe g
                                                                   , decltpeGAS :: t'
                                                                   , bodyGAS :: t } -> SmGivenAliasS m n p p' t' b' t pc pc' g

data SmMacroS m n tn p p' t' b' t pc pc' g where
    SmMacroS :: ( NameT tn
                , ParamClauseGroup m n p p' t' b' t pc pc' g
                ) => { modsMS :: [m]
                     , nameMS :: tn
                     , paramClauseGroupsMS :: [g]
                     , decltpeOptMS :: Maybe t'
                     , bodyMS :: t } -> SmMacroS m n tn p p' t' b' t pc pc' g

data SmObjectS m n tn t' t ac i f s e where
    SmObjectS :: ( NameT tn
                 , Template m n t' t ac i f s e
                 ) => { modsOS :: [m]
                      , nameOS :: tn
                      , templOS :: e } -> SmObjectS m n tn t' t ac i f s e

data SmRepeatedEnumCase m tn where
    SmRepeatedEnumCase :: ( Mod m
                          , NameT tn
                          ) => { modsRECS :: [m]
                               , cases :: [tn] } -> SmRepeatedEnumCase m tn

data SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e where
    SmTraitS :: ( NameT' t'n
                , ParamClauseT' m n p' t' b' pc'
                , Primary m n p t' t pc c
                , Template m n t' t ac i f s e
                ) => { modsTS :: [m]
                     , nameTS :: t'n
                     , t'paramClauseTS :: pc'
                     , ctorTS :: c
                     , templTS :: e } -> SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e

data SmTypeS m n t'n p' t' b' pc' where
    SmTypeS :: ( NameT' t'n
               , ParamClauseT' m n p' t' b' pc'
               ) => { modsTpeS :: [m]
                    , nameTpeS :: t'n
                    , t'paramClauseTpeS :: pc'
                    , bodyTpeS :: t'
                    , boundsTpeS :: b' } -> SmTypeS m n t'n p' t' b' pc'

data SmValS m p t' t where
    SmValS :: ( Mod m
              , Pat p
              , Type' t'
              , Term t
              ) => { modsValS :: [m]
                   , patsValS :: [p]
                   , decltpeOptValS :: Maybe t'
                   , rhsValS :: t } -> SmValS m p t' t

data SmVarS m p t' t where
    SmVarS :: ( Mod m
              , Pat p
              , Type' t'
              , Term t
              ) => { modsVarS :: [m]
                   , patsVarS :: [p]
                   , decltpeOptVarS :: Maybe t'
                   , rhsVarS :: t } -> SmVarS m p t' t

-- I ---------------------------------------------------------------------------

data SmImportExportStatS r i t where
    SmImportS :: Importer r i t => { importersIIESS :: [t] } -> SmImportExportStatS r i t
    SmExportS :: Importer r i t => { importersIEESS :: [t] } -> SmImportExportStatS r i t

-- S ---------------------------------------------------------------------------

data SmPkgS r s where
    SmPkgS :: ( RefT r
              , Stat s
              ) => { refPkS :: r
                   , statsPkS :: [s] } -> SmPkgS r s

data SmPkgObjectS m n tn t' t ac i f s e where
    SmPkgObjectS :: ( NameT tn
                    , Template m n t' t ac i f s e
                    ) => { modsPkObjS :: [m]
                         , namePkObjS :: tn
                         , templPkObjS :: e } -> SmPkgObjectS m n tn t' t ac i f s e
