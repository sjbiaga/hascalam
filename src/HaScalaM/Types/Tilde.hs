module HaScalaM.Types.Tilde where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Enums
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Classes
import HaScalaM.Types.Base
import HaScalaM.Types.Enums
import HaScalaM.Types.Pat
import HaScalaM.Types.Ref
import HaScalaM.Types.Stat
import HaScalaM.Types.Term
import HaScalaM.Types.Type


-- C ---------------------------------------------------------------------------

data SmCaseCT p t where
    SmCaseC :: ( p ~ SmPat
               , t ~ SmTerm
               , Pat p
               , Term t
               ) => { patCCT :: p
                    , condCCT :: Maybe t
                    , bodyCCT :: t } -> SmCaseCT p t

data SmType'CaseCT t' where
    SmType'CaseT'C :: ( t' ~ SmType'
                      , Type' t'
                      ) => { patT'CCT :: t'
                           , bodyT'CCT :: t' } -> SmType'CaseCT t'


data SmCtorPrimary m n p t' t pc where
    SmCtorPrimary :: ( m ~ SmMod
                     , n ~ SmName
                     , p ~ SmParamT m n t' t
                     , pc ~ SmParamClauseT m n p t' t
                     , ParamClauseT m n p t' t pc
                     ) => { modsCP :: [m]
                          , nameCP :: n
                          , paramClausesCP :: [pc] } -> SmCtorPrimary m n p t' t pc

-- E ---------------------------------------------------------------------------

data SmEnumerator where
    ECaseGenerator :: ( p ~ SmPat
                      , b ~ SmTerm
                      , Pat p
                      , Term b
                      ) => SmCaseGeneratorE p b -> SmEnumerator
    EGenerator :: ( p ~ SmPat
                  , b ~ SmTerm
                  , Pat p
                  , Term b
                  ) => SmGeneratorE p b -> SmEnumerator
    EGuard :: ( b ~ SmTerm
              , Term b
              ) => SmGuardE b -> SmEnumerator
    EVal :: ( p ~ SmPat
            , b ~ SmTerm
            , Pat p
            , Term b
            ) => SmValE p b -> SmEnumerator

-- I ---------------------------------------------------------------------------

data SmImportee where
    SmGivenI :: ( t' ~ SmType'
                , Type' t'
                ) => { tpeGI :: t' } -> SmImportee
    SmGivenAllI :: SmImportee
    SmNameI :: ( n ~ SmName
               , Name n
               )  => { nameNI :: n } -> SmImportee
    SmRenameI :: ( n ~ SmName
                 , Name n
                 ) => { nameRI :: n
                      , renameRI :: n } -> SmImportee
    SmUnimportI ::( n ~ SmName
                  , Name n
                  ) => { nameUI :: n } -> SmImportee
    SmWildcardI :: SmImportee


data SmImporter r i where
    SmImporter :: ( r ~ SmRef
                  , i ~ SmImportee
                  , RefT r
                  , Importee i
                  ) => { refII :: r
                       , importeesII :: [i] } -> SmImporter r i


data SmInit m n t' t ac where
    SmInit :: ( m ~ SmMod
              , n ~ SmName
              , t' ~ SmType'
              , t ~ SmTerm
              , ac ~ SmArgClauseT m t
              , Name n
              , Type' t'
              , ArgClauseT m t ac
              ) => { tpeI :: t'
                   , nameI :: n
                   , argClausesI :: [ac] } -> SmInit m n t' t ac

-- M ---------------------------------------------------------------------------

data SmMod where
    MAnnot :: ( m ~ SmMod
              , n ~ SmName
              , t' ~ SmType'
              , t ~ SmTerm
              , ac ~ SmArgClauseT m t
              , i ~ SmInit m n t' t ac
              , Init m n t' t ac i
              ) => SmAnnotM m n t' t ac i -> SmMod
    MMod :: SmModM -> SmMod
    MAccess :: ( r ~ SmRef_
               , Ref r
               ) => SmAccessM r -> SmMod

-- N ---------------------------------------------------------------------------

data SmName where
    NAnonymous :: SmAnonymousRT -> SmName
    NName :: SmNameN -> SmName
    NTName :: SmNameT -> SmName
    NT'Name :: SmNameT' -> SmName


data SmNameT where
    SmNameT :: { valueNT :: String } -> SmNameT


data SmNameT' where
    SmNameT' :: { valueNT' :: String } -> SmNameT'

-- P ---------------------------------------------------------------------------

data SmParamClauseGroup m n p p' t' b' t pc pc' where
    SmParamClauseGroup :: ( m ~ SmMod
                          , n ~ SmName
                          , p ~ SmParamT m n t' t
                          , p' ~ SmParamT' m n t' b'
                          , t' ~ SmType'
                          , b' ~ SmBounds' t'
                          , t ~ SmTerm
                          , pc ~ SmParamClauseT m n p t' t
                          , pc' ~ SmParamClauseT' m n p' t' b'
                          , ParamClauseT' m n p' t' b' pc'
                          , ParamClauseT m n p t' t pc
                          ) => { t'paramClausePCG :: pc'
                               , paramClausesPCG :: [pc] } -> SmParamClauseGroup m n p p' t' b' t pc pc'

data SmPatP where
    SmAlternativeP :: ( p ~ SmPat
                      , Pat p
                      ) => { lhsAP :: p
                           , rhsAP :: p } -> SmPatP
    SmBindP :: ( p ~ SmPat
               , Pat p
               ) => { lhsBP :: p
                    , rhsBP :: p } -> SmPatP
    SmGivenP :: ( t' ~ SmType'
                , Type' t'
                ) => { tpeGP :: t' } -> SmPatP
    SmInterpolateP :: ( tn ~ SmNameT
                      , l ~ SmLit
                      , p ~ SmPat
                      , NameT tn
                      , Lit l
                      , Pat p
                      ) => { prefixIP :: tn
                           , partsIP :: [l]
                           , argsIP :: [p] } -> SmPatP
    SmNameP :: ( tn ~ SmNameT
               , NameT tn
               ) => { nameP :: tn } -> SmPatP
    SmRepeatedP :: ( tn ~ SmNameT
                   , NameT tn
                   ) => { nameRP :: tn } -> SmPatP
    SmSelectP :: ( tn ~ SmNameT
                 , t ~ SmTerm
                 , NameT tn
                 , Term t
                 ) => { seleP :: SmSelectRT tn t } -> SmPatP
    SmSeqWildcardP :: SmPatP
    SmTypedP :: ( p ~ SmPat
                , t' ~ SmType'
                , Type' t'
                , Pat p
                , Type' t'
                ) => { lhsTP :: p
                     , rhsTP :: t' } -> SmPatP
    SmWildcardP :: SmPatP
    SmXmlP :: ( l ~ SmLit
              , p ~ SmPat
              , Lit l
              , Pat p
              ) => { partsXP :: [l]
                   , argsXP :: [p] } -> SmPatP

data SmPat where
    PExtract :: ( t ~ SmTerm
                , p ~ SmPat
                , ac ~ SmArgClauseP p
                , Term t
                , Pat p
                , ArgClauseP p ac
                ) => SmExtractP t p ac -> SmPat
    PExtractInfix :: ( tn ~ SmNameT
                     , p ~ SmPat
                     , ac ~ SmArgClauseP p
                     , NameT tn
                     , Pat p
                     , ArgClauseP p ac
                     ) => SmExtractInfixP tn p ac -> SmPat
    PLit :: SmLit -> SmPat
    PMacro :: ( t ~ SmTerm
              , Term t
              ) => SmMacroP t -> SmPat
    PPat :: SmPatP -> SmPat
    PTuple :: ( p ~ SmPat
              , Pat p
              ) => SmTupleP p -> SmPat
    PVar :: ( tn ~ SmNameT
            , NameT tn
            ) => SmVarP tn -> SmPat

data SmArgClauseP p where
    SmArgClauseP :: ( p ~ SmPat
                    , Pat p
                    ) => { valuesACP :: [p] } -> SmArgClauseP p

-- R ---------------------------------------------------------------------------

data SmRef_ where
    RImportee :: SmImportee -> SmRef_
    RInit :: ( m ~ SmMod
             , n ~ SmName
             , t' ~ SmType'
             , t ~ SmTerm
             , ac ~ SmArgClauseT m t
             , Name n
             , Type' t'
             , ArgClauseT m t ac
             ) => SmInit m n t' t ac -> SmRef_
    RName :: SmName -> SmRef_
    R_TRef :: SmRef -> SmRef_
    R_T'Ref:: SmRef' -> SmRef_


data SmRef where
    RTAnonymous :: SmAnonymousRT -> SmRef
    RTName :: SmNameT -> SmRef
    RTRef :: ( n ~ SmNameT
             , t ~ SmTerm
             ) => SmRefT n t -> SmRef
    RTSelect :: ( tn ~ SmNameT
                , t ~ SmTerm
                , Name tn
                , Term t
                ) => SmSelectRT tn t -> SmRef


data SmRef' where
    RT'Name :: SmNameT' -> SmRef'
    RT'Ref :: ( t'n ~ SmNameT'
              , t' ~ SmType'
              , r ~ SmRef
              ) => SmRefT' t'n t' r -> SmRef'

-- S ---------------------------------------------------------------------------

data SmSelf n t' where
    SmSelf :: ( n ~ SmName
              , t' ~ SmType'
              , Name n
              , Type' t'
              ) => { nameS :: n
                   , decltpeOptS :: Maybe t' } -> SmSelf n t'


data SmStat where
    SCtorSecondary :: ( m ~ SmMod
                      , n ~ SmName
                      , p ~ SmParamT m n t' t
                      , t' ~ SmType'
                      , t ~ SmTerm
                      , pc ~ SmParamClauseT m n p t' t
                      , ac ~ SmArgClauseT m t
                      , i ~ SmInit m n t' t ac
                      , s ~ SmStat
                      , ParamClauseT m n p t' t pc
                      , Init m n t' t ac i
                      , Stat s
                      ) => SmCtorSecondaryS m n p t' t pc ac i s -> SmStat
    SDef' :: ( m ~ SmMod
             , n ~ SmName
             , tn ~ SmNameT
             , p ~ SmParamT m n t' t
             , p' ~ SmParamT' m n t' b'
             , t' ~ SmType'
             , b' ~ SmBounds' t'
             , t ~ SmTerm
             , pc ~ SmParamClauseT m n p t' t
             , pc' ~ SmParamClauseT' m n p' t' b'
             , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
             , NameT tn
             , ParamClauseGroup m n p p' t' b' t pc pc' g
             ) => SmDef'S m n tn p p' t' b' t pc pc' g -> SmStat
    SGiven' :: ( m ~ SmMod
               , n ~ SmName
               , tn ~ SmNameT
               , p ~ SmParamT m n t' t
               , p' ~ SmParamT' m n t' b'
               , t' ~ SmType'
               , b' ~ SmBounds' t'
               , t ~ SmTerm
               , pc ~ SmParamClauseT m n p t' t
               , pc' ~ SmParamClauseT' m n p' t' b'
               , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
               , NameT tn
               , ParamClauseGroup m n p p' t' b' t pc pc' g
               ) => SmGiven'S m n tn p p' t' b' t pc pc' g -> SmStat
    SType' :: ( m ~ SmMod
              , n ~ SmName
              , t'n ~ SmNameT'
              , p' ~ SmParamT' m n t' b'
              , t' ~ SmType'
              , b' ~ SmBounds' t'
              , pc' ~ SmParamClauseT' m n p' t' b'
              , NameT' t'n
              , ParamClauseT' m n p' t' b' pc'
              ) => SmType'S m n t'n p' t' b' pc' -> SmStat
    SVal' :: ( m ~ SmMod
             , p ~ SmPat
             , t' ~ SmType'
             , Mod m
             , Pat p
             , Type' t'
             ) => SmVal'S m p t' -> SmStat
    SVar' :: ( m ~ SmMod
             , p ~ SmPat
             , t' ~ SmType'
             , Mod m
             , Pat p
             , Type' t'
             ) => SmVar'S m p t' -> SmStat
    SClass :: ( m ~ SmMod
              , n ~ SmName
              , t'n ~ SmNameT'
              , p ~ SmParamT m n t' t
              , p' ~ SmParamT' m n t' b'
              , t' ~ SmType'
              , b' ~ SmBounds' t'
              , t ~ SmTerm
              , pc ~ SmParamClauseT m n p t' t
              , pc' ~ SmParamClauseT' m n p' t' b'
              , c ~ SmCtorPrimary m n p t' t pc
              , ac ~ SmArgClauseT m t
              , i ~ SmInit m n t' t ac
              , f ~ SmSelf n t'
              , s ~ SmStat
              , e ~ SmTemplate m n t' t ac i f s
              , NameT' t'n
              , ParamClauseT' m n p' t' b' pc'
              , Primary m n p t' t pc c
              , Template m n t' t ac i f s e
              ) => SmClassS m n t'n p p' t' b' t pc pc' c ac i f s e -> SmStat
    SDef :: ( m ~ SmMod
            , n ~ SmName
            , tn ~ SmNameT
            , p ~ SmParamT m n t' t
            , p' ~ SmParamT' m n t' b'
            , t' ~ SmType'
            , b' ~ SmBounds' t'
            , t ~ SmTerm
            , pc ~ SmParamClauseT m n p t' t
            , pc' ~ SmParamClauseT' m n p' t' b'
            , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
            , NameT tn
            , ParamClauseGroup m n p p' t' b' t pc pc' g
            ) => SmDefS m n tn p p' t' b' t pc pc' g -> SmStat
    SEnum :: ( m ~ SmMod
             , n ~ SmName
             , t'n ~ SmNameT'
             , p ~ SmParamT m n t' t
             , p' ~ SmParamT' m n t' b'
             , t' ~ SmType'
             , b' ~ SmBounds' t'
             , t ~ SmTerm
             , pc ~ SmParamClauseT m n p t' t
             , pc' ~ SmParamClauseT' m n p' t' b'
             , c ~ SmCtorPrimary m n p t' t pc
             , ac ~ SmArgClauseT m t
             , i ~ SmInit m n t' t ac
             , f ~ SmSelf n t'
             , s ~ SmStat
             , e ~ SmTemplate m n t' t ac i f s
             , NameT' t'n
             , ParamClauseT' m n p' t' b' pc'
             , Primary m n p t' t pc c
             , Template m n t' t ac i f s e
             ) => SmEnumS m n t'n p p' t' b' t pc pc' c ac i f s e -> SmStat
    SEnumCase :: ( m ~ SmMod
                 , n ~ SmName
                 , tn ~ SmNameT
                 , p ~ SmParamT m n t' t
                 , p' ~ SmParamT' m n t' b'
                 , t' ~ SmType'
                 , b' ~ SmBounds' t'
                 , t ~ SmTerm
                 , pc ~ SmParamClauseT m n p t' t
                 , pc' ~ SmParamClauseT' m n p' t' b'
                 , c ~ SmCtorPrimary m n p t' t pc
                 , ac ~ SmArgClauseT m t
                 , i ~ SmInit m n t' t ac
                 , NameT tn
                 , ParamClauseT' m n p' t' b' pc'
                 , Primary m n p t' t pc c
                 , Init m n t' t ac i
                 ) => SmEnumCaseS m n tn p p' t' b' t pc pc' c ac i -> SmStat
    SExtensionGroup :: ( m ~ SmMod
                       , n ~ SmName
                       , p ~ SmParamT m n t' t
                       , p' ~ SmParamT' m n t' b'
                       , t' ~ SmType'
                       , b' ~ SmBounds' t'
                       , t ~ SmTerm
                       , pc ~ SmParamClauseT m n p t' t
                       , pc' ~ SmParamClauseT' m n p' t' b'
                       , s ~ SmStat
                       , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
                       , ParamClauseGroup m n p p' t' b' t pc pc' g
                       , Stat s
                       ) => SmExtensionGroupS m n p p' t' b' t pc pc' s g -> SmStat
    SGiven :: ( m ~ SmMod
              , n ~ SmName
              , tn ~ SmNameT
              , p ~ SmParamT m n t' t
              , p' ~ SmParamT' m n t' b'
              , t' ~ SmType'
              , b' ~ SmBounds' t'
              , t ~ SmTerm
              , pc ~ SmParamClauseT m n p t' t
              , pc' ~ SmParamClauseT' m n p' t' b'
              , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
              , ac ~ SmArgClauseT m t
              , i ~ SmInit m n t' t ac
              , f ~ SmSelf n t'
              , s ~ SmStat
              , e ~ SmTemplate m n t' t ac i f s
              , ParamClauseGroup m n p p' t' b' t pc pc' g
              , Template m n t' t ac i f s e
              ) => SmGivenS m n p p' t' b' t pc pc' ac i f s e g -> SmStat
    SImpExp :: ( r ~ SmRef
               , i ~ SmImportee
               , t ~ SmImporter r i
               , Importer r i t
               ) => SmImportExportStatS r i t -> SmStat
    SGivenAlias :: ( m ~ SmMod
                   , n ~ SmName
                   , p ~ SmParamT m n t' t
                   , p' ~ SmParamT' m n t' b'
                   , t' ~ SmType'
                   , b' ~ SmBounds' t'
                   , t ~ SmTerm
                   , pc ~ SmParamClauseT m n p t' t
                   , pc' ~ SmParamClauseT' m n p' t' b'
                   , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
                   , ParamClauseGroup m n p p' t' b' t pc pc' g
                   ) => SmGivenAliasS m n p p' t' b' t pc pc' g -> SmStat
    SMacro :: ( m ~ SmMod
              , n ~ SmName
              , tn ~ SmNameT
              , p ~ SmParamT m n t' t
              , p' ~ SmParamT' m n t' b'
              , t' ~ SmType'
              , b' ~ SmBounds' t'
              , t ~ SmTerm
              , pc ~ SmParamClauseT m n p t' t
              , pc' ~ SmParamClauseT' m n p' t' b'
              , g ~ SmParamClauseGroup m n p p' t' b' t pc pc'
              , NameT tn
              , ParamClauseGroup m n p p' t' b' t pc pc' g
              ) => SmMacroS m n tn p p' t' b' t pc pc' g -> SmStat
    SObject :: ( m ~ SmMod
               , n ~ SmName
               , tn ~ SmNameT
               , p ~ SmParamT m n t' t
               , t' ~ SmType'
               , t ~ SmTerm
               , ac ~ SmArgClauseT m t
               , i ~ SmInit m n t' t ac
               , f ~ SmSelf n t'
               , s ~ SmStat
               , e ~ SmTemplate m n t' t ac i f s
               , NameT tn
               , Template m n t' t ac i f s e
               ) => SmObjectS m n tn t' t ac i f s e -> SmStat
    SPkg :: ( r ~ SmRef
            , s ~ SmStat
            , RefT r
            , Stat s
            ) => SmPkgS r s-> SmStat
    SPkgObject :: ( m ~ SmMod
                  , n ~ SmName
                  , tn ~ SmNameT
                  , p ~ SmParamT m n t' t
                  , t' ~ SmType'
                  , t ~ SmTerm
                  , ac ~ SmArgClauseT m t
                  , i ~ SmInit m n t' t ac
                  , f ~ SmSelf n t'
                  , s ~ SmStat
                  , e ~ SmTemplate m n t' t ac i f s
                  , NameT tn
                  , Template m n t' t ac i f s e
                  ) => SmPkgObjectS m n tn t' t ac i f s e -> SmStat
    SRepeatedEnumCase :: ( m ~ SmMod
                         , tn ~ SmNameT
                         , Mod m
                         , NameT tn
                         ) => SmRepeatedEnumCase m tn -> SmStat
    STerm :: SmTerm -> SmStat
    STrait :: ( m ~ SmMod
              , n ~ SmName
              , t'n ~ SmNameT'
              , p ~ SmParamT m n t' t
              , p' ~ SmParamT' m n t' b'
              , t' ~ SmType'
              , b' ~ SmBounds' t'
              , t ~ SmTerm
              , pc ~ SmParamClauseT m n p t' t
              , pc' ~ SmParamClauseT' m n p' t' b'
              , c ~ SmCtorPrimary m n p t' t pc
              , ac ~ SmArgClauseT m t
              , i ~ SmInit m n t' t ac
              , f ~ SmSelf n t'
              , s ~ SmStat
              , e ~ SmTemplate m n t' t ac i f s
              , NameT' t'n
              , ParamClauseT' m n p' t' b' pc'
              , Primary m n p t' t pc c
              , Template m n t' t ac i f s e
              ) => SmTraitS m n t'n p p' t' b' t pc pc' c ac i f s e -> SmStat
    SType :: ( m ~ SmMod
             , n ~ SmName
             , t'n ~ SmNameT'
             , p' ~ SmParamT' m n t' b'
             , t' ~ SmType'
             , b' ~ SmBounds' t'
             , pc' ~ SmParamClauseT' m n p' t' b'
             , NameT' t'n
             , ParamClauseT' m n p' t' b' pc'
             ) => SmTypeS m n t'n p' t' b' pc' -> SmStat
    SVal :: ( m ~ SmMod
            , p ~ SmPat
            , t' ~ SmType'
            , t ~ SmTerm
            , Mod m
            , Pat p
            , Type' t'
            , Term t
            ) => SmValS m p t' t -> SmStat
    SVar :: ( m ~ SmMod
            , p ~ SmPat
            , t' ~ SmType'
            , t ~ SmTerm
            , Mod m
            , Pat p
            , Type' t'
            , Term t
            ) => SmVarS m p t' t -> SmStat

data SmSource s where
  SmSource :: ( s ~ SmStat
              , Stat s
              ) => { statsS :: [s] } -> SmSource s

-- T ---------------------------------------------------------------------------

data SmTemplate m n t' t ac i p s where
    SmTemplate :: ( m ~ SmMod
                  , n ~ SmName
                  , t' ~ SmType'
                  , t ~ SmTerm
                  , ac ~ SmArgClauseT m t
                  , i ~ SmInit m n t' t ac
                  , s ~ SmStat
                  , p ~ SmSelf n t'
                  , Init m n t' t ac i
                  , Stat s
                  , Self n t' p
                  ) => { earlyT :: [s]
                       , initsT :: [i]
                       , selfT :: p
                       , statsT :: [s]
                       , derivesT :: [t'] } -> SmTemplate m n t' t ac i p s

-- Te --------------------------------------------------------------------------

data SmTermT where
    SmAnnotateT :: ( m ~ SmMod
                   , n ~ SmName
                   , t' ~ SmType'
                   , t ~ SmTerm
                   , ac ~ SmArgClauseT m t
                   , i ~ SmInit m n t' t ac
                   , n ~ SmName
                   , a ~ SmAnnotM m n t' t ac i
                   , Annot m n t' t ac i a
                   ) => { exprAnnT :: t
                        , annotsAnnT :: [a] } -> SmTermT
    SmAnonymousFunctionT :: ( t ~ SmTerm
                            , Term t
                            ) => { bodyAFT :: t } -> SmTermT
    SmAscribeT :: ( t ~ SmTerm
                  , t' ~ SmType'
                  , Term t
                  , Type' t'
                  ) => { exprAscrT :: t
                       , tpeAscrT :: t' } -> SmTermT
    SmBlockT :: ( s ~ SmStat
                , Stat s
                ) => { statsBlT :: [s] } -> SmTermT
    SmEndMarkerT :: ( tn ~ SmNameT
                    , NameT tn
                    ) => { nameEMT :: tn } -> SmTermT
    SmEtaT :: ( t ~ SmTerm
              , Term t
              ) => { exprEtaT :: t } -> SmTermT
    SmIfT :: ( m ~ SmMod
             , t ~ SmTerm
             , Mod m
             , Term t
             ) => { condIfT :: t
                  , thenpIfT :: t
                  , elsepIfT :: t
                  , mods :: [m] } -> SmTermT
    SmInterpolateT :: ( tn ~ SmNameT
                      , l ~ SmLit
                      , t ~ SmTerm
                      , NameT tn
                      , Lit l
                      , Term t
                      ) => { prefixIT :: tn
                           , partsIT :: [l]
                           , argsIT :: [t] } -> SmTermT
    SmNewT :: ( m ~ SmMod
              , n ~ SmName
              , t' ~ SmType'
              , t ~ SmTerm
              , ac ~ SmArgClauseT m t
              , i ~ SmInit m n t' t ac
              , Init m n t' t ac i
              ) => { initN :: i } -> SmTermT
    SmPlaceholder :: SmTermT
    SmQuotedMacroExprT :: ( t ~ SmTerm
                          , Term t
                          ) => { bodyQMET :: t } -> SmTermT
    SmQuotedMacroType'T :: ( t' ~ SmType'
                           , Type' t'
                           ) => { tpeQMT'T :: t' } -> SmTermT
    SmRepeatedT :: ( t ~ SmTerm
                   , Term t
                   ) => { exprRepT :: t } -> SmTermT
    SmReturnT :: ( t ~ SmTerm
                 , Term t
                 ) => { exprRetT :: t } -> SmTermT
    SmSplicedMacroExprT :: ( t ~ SmTerm
                           , Term t
                           ) => { bodySMET :: t } -> SmTermT
    SmSplicedMacroPatT :: ( p ~ SmPat
                          , Pat p
                          ) => { patSMPT :: p } -> SmTermT
    SmThrowT :: ( t ~ SmTerm
                , Term t
                ) => { exprThrT :: t } -> SmTermT
    SmTryWithHandlerT :: ( t ~ SmTerm
                         , Term t
                         ) => { exprTWHT :: t
                              , catchpTWHT :: t
                              , finallypTWHT :: t } -> SmTermT
    SmXmlT :: ( l ~ SmLit
              , t ~ SmTerm
              , Lit l
              , Term t
              ) => { partsXT :: [l]
                   , argsXT :: [t] } -> SmTermT

data SmTerm where
    TApply :: ( m ~ SmMod
              , t ~ SmTerm
              , ac ~ SmArgClauseT m t
              , ArgClauseT m t ac
              ) => SmApplyT m t ac -> SmTerm
    TApplyInfix :: ( m ~ SmMod
                   , tn ~ SmNameT
                   , t' ~ SmType'
                   , t ~ SmTerm
                   , ac' ~ SmArgClauseT' t'
                   , ac ~ SmArgClauseT m t
                   , NameT tn
                   , ArgClauseT' t' ac'
                   , ArgClauseT m t ac
                   ) => SmApplyInfixT m tn t t' ac' ac -> SmTerm
    TApplyType' :: ( t ~ SmTerm
                   , t' ~ SmType'
                   , ac' ~ SmArgClauseT' t'
                   , Term t
                   , ArgClauseT' t' ac'
                   ) => SmApplyType'T t t' ac' -> SmTerm
    TAssign :: ( t ~ SmTerm
               , Term t
               ) => SmAssignT t -> SmTerm
    TContextFunction :: ( m ~ SmMod
                        , n ~ SmName
                        , p ~ SmParamT m n t' t
                        , t' ~ SmType'
                        , t ~ SmTerm
                        , pc ~ SmParamClauseT m n p t' t
                        , ParamClauseT m n p t' t pc
                        ) => SmContextFunctionT m n p t' t pc -> SmTerm
    TDo :: ( t ~ SmTerm
           , Term t
           ) => SmDoT t -> SmTerm
    TFor :: ( e ~ SmEnumerator
            , t ~ SmTerm
            , Enumerator e
            , Term t
            ) => SmForT e t -> SmTerm
    TForYield :: ( e ~ SmEnumerator
                 , t ~ SmTerm
                 , Enumerator e
                 , Term t
                 ) => SmForYieldT e t -> SmTerm
    TFunction :: ( m ~ SmMod
                 , n ~ SmName
                 , p ~ SmParamT m n t' t
                 , t' ~ SmType'
                 , t ~ SmTerm
                 , pc ~ SmParamClauseT m n p t' t
                 , ParamClauseT m n p t' t pc
                 ) => SmFunctionT m n p t' t pc -> SmTerm
    TLit :: SmLit -> SmTerm
    TMatch :: ( p ~ SmPat
              , t ~ SmTerm
              , ct ~ SmCaseCT p t
              , Case p t ct
              ) => SmMatchT p t ct -> SmTerm
    TNewAnonymous :: ( m ~ SmMod
                     , n ~ SmName
                     , t' ~ SmType'
                     , t ~ SmTerm
                     , ac ~ SmArgClauseT m t
                     , i ~ SmInit m n t' t ac
                     , p ~ SmSelf n t'
                     , s ~ SmStat
                     , e ~ SmTemplate m n t' t ac i p s
                     , Template m n t' t ac i p s e
                     ) => SmNewAnonymousT m n t' t ac i p s e -> SmTerm
    TPartialFunction :: ( p ~ SmPat
                        , t ~ SmTerm
                        , ct ~ SmCaseCT p t
                        , Case p t ct
                        ) => SmPartialFunctionT p t ct -> SmTerm
    TPolyFunction :: ( m ~ SmMod
                     , n ~ SmName
                     , p' ~ SmParamT' m n t' b'
                     , t' ~ SmType'
                     , b' ~ SmBounds' t'
                     , pc' ~ SmParamClauseT' m n p' t' b'
                     , t ~ SmTerm
                     , ParamClauseT' m n p' t' b' pc'
                     , Term t
                     ) => SmPolyFunctionT m n p' t' b' pc' t -> SmTerm
    TRef :: SmRef -> SmTerm
    TTerm :: SmTermT -> SmTerm
    TTry :: ( p ~ SmPat
            , t ~ SmTerm
            , ct ~ SmCaseCT p t
            , Case p t ct
            ) => SmTryT p t ct -> SmTerm
    TTuple :: ( t ~ SmTerm
              , Term t
              ) => SmTupleT t -> SmTerm
    TWhile :: ( t ~ SmTerm
              , Term t
              ) => SmWhileT t -> SmTerm

data SmArgClauseT m t where
    SmArgClauseT :: ( m ~ SmMod
                    , t ~ SmTerm
                    , ArgsType m
                    , Term t
                    ) => { valuesACT :: [t]
                         , modACT :: Maybe m } -> SmArgClauseT m t

data SmParamT m n t' t where
    SmParamT :: ( m ~ SmMod
                , n ~ SmName
                , t' ~ SmType'
                , t ~ SmTerm
                , Mod m
                , Name n
                , Type' t'
                , Term t
                ) => { modsPT :: [m]
                     , namePT :: n
                     , decltpeOptPT :: Maybe t'
                     , defaultOptPT :: Maybe t } -> SmParamT m n t' t

data SmParamClauseT m n p t' t where
    SmParamClauseT :: ( p ~ SmParamT m n t' t
                      , ParamsType m
                      , ParamT m n p t' t
                      ) => { valuesPCT :: [p]
                           , modPCT :: Maybe m } -> SmParamClauseT m n p t' t

-- Ty --------------------------------------------------------------------------

data SmType'T' where
    SmAndT' :: ( t' ~ SmType'
               , Type' t'
               ) => { lhsAndT' :: t'
                    , rhsAndT' :: t' } -> SmType'T'
    SmAnnotateT' :: ( m ~ SmMod
                    , n ~ SmName
                    , t' ~ SmType'
                    , ac ~ SmArgClauseT m t
                    , a ~ SmAnnotM m n t' t ac i
                    , i ~ SmInit m n t' t ac
                    , Annot m n t' t ac i a
                    ) => { tpeAnnT' :: t'
                         , annotsAnnT' :: [a] } -> SmType'T'
    SmAnonymousLambdaT' :: ( t' ~ SmType'
                           , Type' t'
                           ) => { tpeALT' :: t' } -> SmType'T'
    SmAnonymousNameT' :: SmType'T'
    SmAnonymousParamT' :: ( m ~ SmMod
                          , Variant m
                          ) => { variantAPT' :: Maybe m } -> SmType'T'
    SmBlockT' :: ( m ~ SmMod
                 , n ~ SmName
                 , t'n ~ SmNameT'
                 , p' ~ SmParamT' m n t' b'
                 , t' ~ SmType'
                 , b' ~ SmBounds' t'
                 , pc' ~ SmParamClauseT' m n p' t' b'
                 , t'd ~ SmType'Def m n t'n p' t' b' pc'
                 , Type'Def m n t'n p' t' b' pc' t'd
                 ) => { type'DefsBT' :: [t'd],
                        tpeBT' :: t' } -> SmType'T'
    SmByNameT' :: ( t' ~ SmType'
                  , Type' t'
                  ) => { tpeBNT' :: t' } -> SmType'T'
    SmExistentialT' :: ( t' ~ SmType'
                       , s ~ SmStat
                       , Type' t'
                       , Stat s
                       ) => { tpeET' :: t'
                            , statsET' :: [s] } -> SmType'T'
    SmImplicitFunctionT' :: ( t' ~ SmType'
                            , Type' t'
                            ) => { paramsIFT' :: [t']
                                 , resIFT' :: t' } -> SmType'T'
    SmMethodT' :: ( m ~ SmMod
                  , n ~ SmName
                  , p ~ SmParamT m n t' t
                  , t ~ SmTerm
                  , t' ~ SmType'
                  , pc ~ SmParamClauseT m n p t' t
                  , ParamClauseT m n p t' t pc
                  ) => { paramClausesMT' :: [pc]
                       , tpeMthT' :: t' } -> SmType'T'
    SmOrT' :: ( t' ~ SmType'
              , Type' t'
              ) => { lhsOrT' :: t'
                   , rhsOrT' :: t' } -> SmType'T'
    SmRefineT' :: ( t' ~ SmType'
                  , s ~ SmStat
                  , Type' t'
                  , Stat s
                  ) => { tpeRfT' :: Maybe t'
                       , statsRfT' :: [s] } -> SmType'T'
    SmRepeatedT' :: ( t' ~ SmType'
                    , Type' t'
                    ) => { tpeRpT' :: t' } -> SmType'T'
    SmPatWildcardT' :: SmType'T'
    SmWithT' :: ( t' ~ SmType'
                , Type' t'
                ) => { lhsWT' :: t'
                     , rhsWT' :: t' } -> SmType'T'
    SmWildcardT' :: ( t' ~ SmType'
                    , b' ~ SmBounds' t'
                    , Type' t'
                    , Bounds' t' b'
                    ) => { boundsWT' :: b' } -> SmType'T'

data SmType' where
    T'Apply :: ( n ~ SmName
               , t' ~ SmType'
               , ac' ~ SmArgClauseT' t'
               , ArgClauseT' t' ac'
               ) => SmApplyT' t' ac' -> SmType'
    T'ApplyInfix :: ( t'n ~ SmNameT'
                    , t' ~ SmType'
                    , NameT' t'n
                    , Type' t'
                    ) => SmApplyInfixT' t'n t' -> SmType'
    T'ContextFunction :: ( t' ~ SmType'
                         , Type' t'
                         ) => SmContextFunctionT' t' -> SmType'
    T'Function :: ( t' ~ SmType'
                  , Type' t'
                  ) => SmFunctionT' t' -> SmType'
    T'Lambda :: ( m ~ SmMod
                , n ~ SmName
                , p' ~ SmParamT' m n t' b'
                , t' ~ SmType'
                , b' ~ SmBounds' t'
                , pc' ~ SmParamClauseT' m n p' t' b'
                , ParamClauseT' m n p' t' b' pc'
                ) => SmLambdaT' m n p' t' b' pc' -> SmType'
    T'Lit :: SmLit -> SmType'
    T'Macro :: ( t ~ SmTerm
               , Term t
               ) => SmMacroT' t -> SmType'
    T'Match :: ( t' ~ SmType'
               , ct ~ SmType'CaseCT t'
               , Type'Case t' ct
               ) => SmMatchT' t' ct -> SmType'
    T'PolyFunction :: ( m ~ SmMod
                      , n ~ SmName
                      , p' ~ SmParamT' m n t' b'
                      , t' ~ SmType'
                      , b' ~ SmBounds' t'
                      , pc' ~ SmParamClauseT' m n p' t' b'
                      , ParamClauseT' m n p' t' b' pc'
                      ) => SmPolyFunctionT' m n p' t' b' pc' -> SmType'
    T'Ref :: SmRef' -> SmType'
    T'Tuple :: ( t' ~ SmType'
               , Type' t'
               ) => SmTupleT' t' -> SmType'
    T'Type' :: SmType'T' -> SmType'
    T'Var :: ( t'n ~ SmNameT'
             , NameT' t'n
             ) => SmVarT' t'n -> SmType'

data SmArgClauseT' t' where
    SmArgClauseT' :: ( t' ~ SmType'
                     , Type' t'
                     ) => { valuesACACT' :: [t'] } -> SmArgClauseT' t'


data SmBounds' t' where
    SmBounds' :: ( t' ~ SmType'
                 , Type' t'
                 ) => { loB' :: Maybe t'
                      , hiB' :: Maybe t' } -> SmBounds' t'


data SmType'Def m n t'n p' t' b' pc' where
    T'DType' :: SmType'S m n t'n p' t' b' pc' -> SmType'Def m n t'n p' t' b' pc'
    T'DType :: SmTypeS m n t'n p' t' b' pc' -> SmType'Def m n t'n p' t' b' pc'


data SmParamT' m n t' b' where
    SmParamT' :: ( n ~ SmName
                 , p' ~ SmParamT' m n t' b'
                 , pc' ~ SmParamClauseT' m n p' t' b'
                 , Mod m
                 , Name n
                 , Bounds' t' b'
                 ) => { modsPT' :: [m]
                      , namePT' :: n
                      , t'paramClausePT' :: pc'
                      , tboundsPT' :: b'
                      , vboundsPT' :: [t']
                      , cboundsPT' :: [t'] } -> SmParamT' m n t' b'


data SmParamClauseT' m n p' t' b' where
    SmParamClauseT' :: ( p' ~ SmParamT' m n t' b'
                       , Mod m
                       , Name n
                       , Bounds' t' b'
                       ) => { valuesPCT' :: [p'] } -> SmParamClauseT' m n p' t' b'
