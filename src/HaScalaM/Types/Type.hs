{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Types.Type where

import HaScalaM.Classes.Term
import HaScalaM.Classes.Type


-- T ---------------------------------------------------------------------------

data SmApplyT' t' ac' where
    SmApplyT' :: ArgClauseT' t' ac' => { tpeAT' :: t'
                                       , argClauseAT' :: ac' } -> SmApplyT' t' ac'

data SmApplyInfixT' t'n t' where
    SmApplyInfixT' :: ( NameT' t'n
                      , Type' t'
                      ) => { lhsAIxT' :: t'
                           , opAIxT' :: t'n
                           , rhsAIxT' :: t' } -> SmApplyInfixT' t'n t'

data SmContextFunctionT' t' where
    SmContextFunctionT' :: Type' t' => { paramClauseCFT' :: FuncParamClause' t'
                                       , resCFT' :: t' } -> SmContextFunctionT' t'

data SmFunctionT' t' where
    SmFunctionT' :: Type' t' => { paramClauseFT' :: FuncParamClause' t'
                                , resFT' :: t' } -> SmFunctionT' t'

-- data SmFunctionArgT' m t' where
--     SmFunctionArgT' :: ( Mod m
--                        , Type' t'
--                        ) => { modsFAT' :: [m]
--                             , tpeFAT' :: t' } -> SmFunctionArgT' m t'

data SmLambdaT' m n p' t' b' pc' where
    SmLambdaT' :: ParamClauseT' m n p' t' b' pc' => { t'paramClauseLT' :: pc'
                                                    , tpeLT' :: t' } -> SmLambdaT' m n p' t' b' pc'

data SmMacroT' t where
    SmMacroT' :: Term t => { bodyMcrT' :: t } -> SmMacroT' t

data SmMatchT' t' ct where
    SmMatchT' :: Type'Case t' ct => { tpeMtchT' :: t'
                                    , casesMtchT' :: [ct] } -> SmMatchT' t' ct

data SmPolyFunctionT' m n p' t' b' pc' where
    SmPolyFunctionT' :: ParamClauseT' m n p' t' b' pc' => { t'ParamClausePFT' :: pc'
                                                          , bodyPFT' :: t' } -> SmPolyFunctionT' m n p' t' b' pc'

data SmTupleT' t' where
    SmTupleT' :: Type' t' => { argsTT' :: [t'] } -> SmTupleT' t'


data SmVarT' t'n where
    SmVarT' :: NameT' t'n => { nameVT' :: t'n } -> SmVarT' t'n

-- data SmTypedParamT' t'n t' where
--     SmTypedParamT' :: ( NameT' t'n
--                       , Type' t'
--                       ) => { nameTPT' :: t'n
--                            , tpeTPT' :: t' } -> SmTypedParamT' t'n t'

