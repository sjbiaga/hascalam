{-# LANGUAGE AllowAmbiguousTypes #-}
module HaScalaM.Types.Base where

import Data.Int (Int8, Int16, Int64)
import HaScalaM.Classes.Base
import HaScalaM.Classes.Term
import HaScalaM.Classes


--------------------------------------------------------------------------- A --

data SmAnnotM m n t' t ac i where
    SmAnnotM :: Init m n t' t ac i => { initA :: i } -> SmAnnotM m n t' t ac i

--------------------------------------------------------------------------- C --

data SmCtorSecondaryS m n p t' t pc ac i s where
    SmCtorSecondaryS :: ( ParamClauseT m n p t' t pc
                        , Init m n t' t ac i
                        , Stat s
                        ) => { modsCSS :: [m]
                             , nameCSS :: n
                             , paramsClausesCSS :: [pc]
                             , initCSS :: i
                             , statsCSS :: [s] } -> SmCtorSecondaryS m n p t' t pc ac i s

--------------------------------------------------------------------------- L --

data SmLit where
    SmBooleanL :: { valueBL :: Bool } -> SmLit
    SmByteL :: { valueByL :: Int8 } -> SmLit
    SmCharL :: { valueChL :: Char } -> SmLit
    SmDoubleL :: { valueDL :: Double } -> SmLit
    SmFloatL :: { valueFL :: Float } -> SmLit
    SmIntL :: { valueIL :: Int } -> SmLit
    SmLongL :: { valueLL :: Int64 } -> SmLit
    SmNullL :: SmLit
    SmShortL :: { valueShL :: Int16 } -> SmLit
    SmStringL :: { valueSL :: String } -> SmLit
    SmSymbolL :: { valueSyL :: Symbol } -> SmLit
    SmUnitL :: SmLit

--------------------------------------------------------------------------- M --

data SmModM where
    SmAbstractM :: SmModM
    SmCaseM :: SmModM
    SmCovariantM :: Variant m => SmModM
    SmContravariantM :: Variant m => SmModM
    SmErasedM :: SmModM
    SmFinalM :: SmModM
    SmImplicitM :: ParamsType m => SmModM
    SmInfixM :: SmModM
    SmInlineM :: SmModM
    SmLazyM :: SmModM
    SmOpaqueM :: SmModM
    SmOpenM :: SmModM
    SmOverrideM :: SmModM
    SmSealedM :: SmModM
    SmSuperM :: SmModM
    SmTransparentM :: SmModM
    SmUsingM :: (ArgsType m, ParamsType m) => SmModM
    SmValParamM :: SmModM
    SmVarParamM :: SmModM

data SmAccessM r where
    SmPrivateM :: Ref r => { withinPrivAM :: r } -> SmAccessM r
    SmProtectedM :: Ref r => { withinProtAM :: r } -> SmAccessM r

--------------------------------------------------------------------------- N --

data SmNameN where
    SmAnonymousN :: SmNameN
    SmIndeterminateN :: { valueNI :: String } -> SmNameN
    SmPlaceholderN :: SmNameN
    SmThisN :: SmNameN

--------------------------------------------------------------------------- S --

newtype Symbol = Symbol { name :: String }
