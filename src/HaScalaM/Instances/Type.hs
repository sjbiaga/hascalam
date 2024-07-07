{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Type where

import HaScalaM.Classes.Base
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Stat
import HaScalaM.Types.Type
import HaScalaM.Types.Tilde


--------------------------------------------------------------------------- C --

instance Type' t' => Tree (SmType'CaseCT t')
instance Type' t' => WithBody t' (SmType'CaseCT t')
    where body (SmType'CaseT'C _ b) = b
instance Type' t' => CaseTree t' t' (SmType'CaseCT t')
    where pat (SmType'CaseT'C p _) = p
instance Type' t' => Type'Case t' (SmType'CaseCT t')

--------------------------------------------------------------------------- F --

instance Type' t' => Tree (FuncParamClause' t')
instance Type' t' => SyntaxValuesClauses t' (FuncParamClause' t')
    where values (FuncParamClause' vs) = vs

--------------------------------------------------------------------------- T --

instance Type' t' => Tree (SmArgClauseT' t')
instance Type' t' => SyntaxValuesClauses t' (SmArgClauseT' t')
    where values (SmArgClauseT' vs) = vs
instance Type' t' => ArgClause t' (SmArgClauseT' t')
instance Type' t' => ArgClauseT' t' (SmArgClauseT' t')


instance Type' t' => Tree (SmBounds' t')
instance Type' t' => Bounds' t' (SmBounds' t')
    where lo (SmBounds' l _) = l
          hi (SmBounds' _ h) = h


instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Tree (SmType'Def m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Member t'n (SmType'Def m n t'n p' t' b' pc')
    where name (T'DType' (SmType'S _ n _ _)) = n
          name (T'DType (SmTypeS _ n _ _ _)) = n
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => MemberT' t'n (SmType'Def m n t'n p' t' b' pc')
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithMods m (SmType'Def m n t'n p' t' b' pc')
    where mods (T'DType' (SmType'S ms _ _ _)) = ms
          mods (T'DType (SmTypeS ms _ _ _ _)) = ms
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => WithT'ParamClause m n p' t' b' pc' (SmType'Def m n t'n p' t' b' pc')
    where t'paramClause (T'DType' (SmType'S _ _ pc _)) = pc
          t'paramClause (T'DType (SmTypeS _ _ pc _ _)) = pc
instance ( NameT' t'n
         , ParamClauseT' m n p' t' b' pc'
         ) => Type'Def m n t'n p' t' b' pc' (SmType'Def m n t'n p' t' b' pc')
    where bounds' (T'DType' (SmType'S _ _ _ bs)) = bs
          bounds' (T'DType (SmTypeS _ _ _ _ bs)) = bs


instance ArgClauseT' t' ac' => Tree (SmApplyT' t' ac')
instance ArgClauseT' t' ac' => Apply t' t' ac' (SmApplyT' t' ac')
    where fun (SmApplyT' t' _) = t'
          argClause (SmApplyT' _ ac) = ac

instance ( NameT' t'n
         , Type' t'
         ) => Tree (SmApplyInfixT' t'n t')
instance ( NameT' t'n
         , Type' t'
         ) => Infix t' t'n t' (SmApplyInfixT' t'n t')
    where lhs (SmApplyInfixT' l _ _) = l
          op  (SmApplyInfixT' _ op _) = op
          arg (SmApplyInfixT' _ _ r) = r

instance Type' t' => Tree (SmContextFunctionT' t')
instance Type' t' => WithBody t' (SmContextFunctionT' t')
    where body (SmContextFunctionT' _ r) = r
instance Type' t' => Function t' (FuncParamClause' t') t' (SmContextFunctionT' t')
    where paramClause (SmContextFunctionT' pc _) = pc
instance ( Type' t'
         ) => FunctionT' t' (SmContextFunctionT' t')
    where res (SmContextFunctionT' _ r) = r

instance ( Type' t'
         , Stat s
         ) => Tree (SmExistentialT' t' s)
instance ( Type' t'
         , Stat s
         ) => WithExprs s (SmExistentialT' t' s)
    where exprs (SmExistentialT' _ ss) = ss
instance ( Type' t'
         , Stat s
         ) => WithStats s (SmExistentialT' t' s)

instance Type' t' => Tree (SmFunctionT' t')
instance Type' t' => WithBody t' (SmFunctionT' t')
    where body (SmFunctionT' _ r) = r
instance Type' t' => Function t' (FuncParamClause' t') t' (SmFunctionT' t')
    where paramClause (SmFunctionT' pc _) = pc
instance ( Type' t'
         ) => FunctionT' t' (SmFunctionT' t')
    where res (SmFunctionT' _ r) = r

instance ParamClauseT' m n p' t' b' pc' => Tree (SmLambdaT' m n p' t' b' pc')
instance ParamClauseT' m n p' t' b' pc' => WithBody t' (SmLambdaT' m n p' t' b' pc')
    where body (SmLambdaT' _ t') = t'
instance ParamClauseT' m n p' t' b' pc' => Function p' pc' t' (SmLambdaT' m n p' t' b' pc')
    where paramClause (SmLambdaT' t'pc _) = t'pc
instance ParamClauseT' m n p' t' b' pc' => WithT'ParamClause m n p' t' b' pc' (SmLambdaT' m n p' t' b' pc')
    where t'paramClause (SmLambdaT' t'pc _) = t'pc

instance Term t => Tree (SmMacroT' t)
instance Term t => WithBody t (SmMacroT' t)
    where body (SmMacroT' b) = b

instance Type'Case t' ct => Tree (SmMatchT' t' ct)
instance ( Tree ct
         , Type'Case t' ct
         ) => WithExprs ct (SmMatchT' t' ct)
    where exprs (SmMatchT' _ cs) = cs
instance Type'Case t' ct => WithCases t' t' ct (SmMatchT' t' ct)

instance ParamClauseT' m n p' t' b' pc' => Tree (SmPolyFunctionT' m n p' t' b' pc')
instance ParamClauseT' m n p' t' b' pc' => WithT'ParamClause m n p' t' b' pc' (SmPolyFunctionT' m n p' t' b' pc')
    where t'paramClause (SmPolyFunctionT' t'pc _) = t'pc

instance ( Type' t'
         , Stat s
         ) => Tree (SmRefineT' t' s)
instance ( Type' t'
         , Stat s
         ) => WithExprs s (SmRefineT' t' s)
    where exprs (SmRefineT' _ ss) = ss
instance ( Type' t'
         , Stat s
         ) => WithStats s (SmRefineT' t' s)

instance Type' t' => Tree (SmTupleT' t')
instance Type' t' => Tuple t' (SmTupleT' t')
    where args (SmTupleT' as) = as

instance NameT' n => Tree (SmVarT' n)
instance NameT' n => Member n (SmVarT' n)
    where name (SmVarT' n) = n
instance NameT' n => MemberT' n (SmVarT' n)


instance Tree SmType'
instance Type' SmType'


instance ( Mod m
         , Name n
         , Bounds' t' b'
         ) => Tree (SmParamT' m n t' b')
instance ( Mod m
         , Name n
         , Bounds' t' b'
         ) => Member n (SmParamT' m n t' b')
    where name (SmParamT' _ n _ _ _ _) = n
instance ( Mod m
         , Name n
         , Bounds' t' b'
         ) => Param m n (SmParamT' m n t' b')
    where mods' (SmParamT' ms _ _ _ _ _) = ms
instance ( p' ~ SmParamT' m n t' b'
         , pc' ~ SmParamClauseT' m n p' t' b'
         , Mod m
         , Name n
         , Bounds' t' b'
         , ParamT' m n p' t' b' pc'
         , ParamClauseT' m n p' t' b' pc'
         ) => WithT'ParamClause m n p' t' b' pc' (SmParamT' m n t' b')
    where t'paramClause (SmParamT' _ _ t'pc _ _ _) = t'pc
instance ( p' ~ SmParamT' m n t' b'
         , pc' ~ SmParamClauseT' m n p' t' b'
         , Mod m
         , Name n
         , Bounds' t' b'
         ) => ParamT' m n (SmParamT' m n t' b') t' b' pc'
    where tbounds' (SmParamT' _ _ _ tbs _ _) = tbs
          vbounds' (SmParamT' _ _ _ _ vbs _) = vbs
          cbounds' (SmParamT' _ _ _ _ _ cbs) = cbs


instance ( Mod m
         , Name n
         , Bounds' t' b'
         ) => Tree (SmParamClauseT' m n p' t' b')
instance ( p' ~ SmParamT' m n t' b'
         , Mod m
         , Name n
         , Bounds' t' b'
         ) => SyntaxValuesClauses p' (SmParamClauseT' m n p' t' b')
    where values (SmParamClauseT' vs) = vs
instance ( p' ~ SmParamT' m n t' b'
         , Mod m
         , Name n
         , Bounds' t' b'
         ) => ParamClause m n p' (SmParamClauseT' m n p' t' b')
instance ( p' ~ SmParamT' m n t' b'
         , Mod m
         , Name n
         , Bounds' t' b'
         ) => ParamClauseT' m n p' t' b' (SmParamClauseT' m n p' t' b')
