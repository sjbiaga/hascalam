{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Types.Term where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Enums
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type


--------------------------------------------------------------------------- T --

data SmApplyT m t ac where
    SmApplyT :: ArgClauseT m t ac => { funAppT :: t
                                     , argClauseAppT :: ac } -> SmApplyT m t ac
    SmApplyUsingT :: ArgClauseT m t ac => { funAppUT :: t
                                          , argClauseAppUT :: ac } -> SmApplyT m t ac

data SmApplyInfixT m tn t t' ac' ac where
    SmApplyInfixT :: ( NameT tn
                     , ArgClauseT' t' ac'
                     , ArgClauseT m t ac
                     ) => { lhsAppIxT :: t
                          , opAppIxT :: tn
                          , t'argClauseAppIx'T :: ac'
                          , argClauseAppIxT :: ac } -> SmApplyInfixT m tn t t' ac' ac

data SmApplyType'T t t' ac' where
    SmApplyType'T :: ( Term t
                     , ArgClauseT' t' ac'
                     ) => { funAppT'T :: t
                          , t'argClauseAppT'T :: ac' } -> SmApplyType'T t t' ac'

data SmAssignT t where
    SmAssignT :: Term t => { lhsAT :: t
                           , rhsAT :: t } -> SmAssignT t

data SmBlockT s where
    SmBlockT  :: Stat s => { statsBlT :: [s] } -> SmBlockT s

data SmContextFunctionT m n p t' t pc where
    SmContextFunctionT :: ParamClauseT m n p t' t pc => { paramClauseCFT :: pc
                                                        , bodyCFT :: t } -> SmContextFunctionT m n p t' t pc

data SmDoT t where
    SmDoT :: Term t => { bodyDT :: t
                       , exprDT :: t } -> SmDoT t

data SmForT e t where
    SmForT :: ( Enumerator e
              , Term t
              ) => { enumsFDT :: [e]
                   , bodyFDT :: t } -> SmForT e t

data SmForYieldT e t where
    SmForYieldT :: ( Enumerator e
                   , Term t
                   ) => { enumsFYT :: [e]
                        , bodyFYT :: t } -> SmForYieldT e t

data SmFunctionT m n p t' t pc where
    SmFunctionT :: ParamClauseT m n p t' t pc => { paramClauseFT :: pc
                                                 , bodyFT :: t } -> SmFunctionT m n p t' t pc

data SmIfT m t where
    SmIfT :: ( Mod m
             , Term t
             ) => { condIfT :: t
                  , thenpIfT :: t
                  , elsepIfT :: t
                  , mods :: [m] } -> SmIfT m t

data SmMatchT p t ct where
    SmMatchT :: Case p t ct => { exprMT :: t
                               , casesMT :: [ct] } -> SmMatchT p t ct

data SmNewAnonymousT m n t' t ac i p s e where
    SmNewAnonymousT :: Template m n t' t ac i p s e => { templNAT :: e } -> SmNewAnonymousT m n t' t ac i p s e

data SmPartialFunctionT p t ct where
    SmPartialFunctionT :: Case p t ct => { casesPFT :: [ct] } -> SmPartialFunctionT p t ct

data SmPolyFunctionT m n p' t' b' pc' t where
    SmPolyFunctionT :: ( ParamClauseT' m n p' t' b' pc'
                       , Term t
                       ) => { t'ParamClausePFT :: pc'
                            , bodyPFT :: t } -> SmPolyFunctionT m n p' t' b' pc' t

data SmTryT p t ct where
    SmTryT :: Case p t ct => { exprTT :: t
                             , catchpTT :: [ct]
                             , finallypTT :: Maybe t } -> SmTryT p t ct

data SmTupleT t where
    SmTupleT :: Term t => { argsTT :: [t] } -> SmTupleT t

data SmWhileT t where
    SmWhileT :: Term t => { exprWT :: t
                          , bodyWT :: t } -> SmWhileT t
