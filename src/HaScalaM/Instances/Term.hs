{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Term where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Enums
import HaScalaM.Classes.Term
import HaScalaM.Classes.Type
import HaScalaM.Types.Term
import HaScalaM.Types.Tilde


-- C ---------------------------------------------------------------------------

instance (Pat p, Term t) => Tree (SmCaseCT p t)
instance (Pat p, Term t) => WithBody t (SmCaseCT p t)
    where body (SmCaseC _ _ b) = b
instance (Pat p, Term t) => CaseTree p t (SmCaseCT p t)
    where pat (SmCaseC p _ _) = p
instance ( p ~ SmPat
         , t ~ SmTerm
         , Pat p
         , Term t
         ) => Case p t (SmCaseCT p t)

-- T ---------------------------------------------------------------------------

instance Tree SmTerm
instance Term SmTerm

instance ArgClauseT m t ac => Tree (SmApplyT m t ac)
instance ArgClauseT m t ac => Apply t t ac (SmApplyT m t ac)
    where fun       (SmApplyT f _) = f
          fun       (SmApplyUsingT f _) = f
          argClause (SmApplyT _ ac) = ac
          argClause (SmApplyUsingT _ ac) = ac

instance ( NameT tn
         , ArgClauseT' t' ac'
         , ArgClauseT m t ac
         ) => Tree (SmApplyInfixT m tn t t' ac' ac)
instance ( NameT tn
         , ArgClauseT' t' ac'
         , ArgClauseT m t ac
         ) => Infix t tn ac (SmApplyInfixT m tn t t' ac' ac)
    where lhs (SmApplyInfixT l _ _ _) = l
          op  (SmApplyInfixT _ n _ _) = n
          arg (SmApplyInfixT _ _ _ ac) = ac

instance ( Term t
         , ArgClauseT' t' ac'
         ) => Tree (SmApplyType'T t t' ac')
instance ( Term t
         , ArgClauseT' t' ac'
         ) => Apply t t' ac' (SmApplyType'T t t' ac')
    where fun       (SmApplyType'T f _) = f
          argClause (SmApplyType'T _ ac') = ac'
instance Term t => Tree (SmAssignT t)
instance Term t => WithBody t (SmAssignT t)
    where body (SmAssignT _ r) = r

instance ParamClauseT m n p t' t pc => Tree (SmContextFunctionT m n p t' t pc)
instance ParamClauseT m n p t' t pc => WithBody t (SmContextFunctionT m n p t' t pc)
    where body (SmContextFunctionT _ b) = b
instance ParamClauseT m n p t' t pc => Function p pc t (SmContextFunctionT m n p t' t pc)
    where paramClause (SmContextFunctionT pc _) = pc

instance Term t => Tree (SmDoT t)
instance Term t => WithBody t (SmDoT t)
    where body (SmDoT b _) = b

instance ( Enumerator e
         , Term t
         ) => Tree (SmForT e t)
instance ( Enumerator e
         , Term t
         ) => WithBody t (SmForT e t)
    where body (SmForT _ b) = b
instance ( Enumerator e
         , Term t
         ) => WithEnums e (SmForT e t)
    where enums (SmForT es _) = es

instance ( Enumerator e
         , Term t
         ) => Tree (SmForYieldT e t)
instance ( Enumerator e
         , Term t
         ) => WithBody t (SmForYieldT e t)
    where body (SmForYieldT _ b) = b
instance ( Enumerator e
         , Term t
         ) => WithEnums e (SmForYieldT e t)
    where enums (SmForYieldT es _) = es

instance ParamClauseT m n p t' t pc => Tree (SmFunctionT m n p t' t pc)
instance ParamClauseT m n p t' t pc => WithBody t (SmFunctionT m n p t' t pc)
    where body (SmFunctionT _ b) = b
instance ParamClauseT m n p t' t pc => Function p pc t (SmFunctionT m n p t' t pc)
    where paramClause (SmFunctionT pc _) = pc
instance ParamClauseT m n p t' t pc => FunctionT m n p pc t' t (SmFunctionT m n p t' t pc)

instance ( Pat p
         , Term t
         , Case p t ct
         ) => Tree (SmMatchT p t ct)
instance ( Pat p
         , Term t
         , Case p t ct
         ) => WithCases p t ct (SmMatchT p t ct)
    where cases (SmMatchT _ cs) = cs

instance Template m n t' t ac i p s e => Tree (SmNewAnonymousT m n t' t ac i p s e)
instance Template m n t' t ac i p s e => WithTemplate m n t' t ac i p s e (SmNewAnonymousT m n t' t ac i p s e)
    where templ (SmNewAnonymousT t) = t

instance ( Pat p
         , Term t
         , Case p t ct
         ) => Tree (SmPartialFunctionT p t ct)
instance ( Pat p
         , Term t
         , Case p t ct
         ) => WithCases p t ct (SmPartialFunctionT p t ct)
    where cases (SmPartialFunctionT cs) = cs

instance ( ParamClauseT' m n p' t' b' pc'
         , Term t
         ) => Tree (SmPolyFunctionT m n p' t' b' pc' t)
instance ( ParamClauseT' m n p' t' b' pc'
         , Term t
         ) => WithBody t (SmPolyFunctionT m n p' t' b' pc' t)
    where body (SmPolyFunctionT _ body) = body
instance ( ParamClauseT' m n p' t' b' pc'
         , Term t
         ) => Function p' pc' t (SmPolyFunctionT m n p' t' b' pc' t)
    where paramClause (SmPolyFunctionT t'pc _) = t'pc
instance ( ParamClauseT' m n p' t' b' pc'
         , Term t
         ) => WithT'ParamClause m n p' t' b' pc' (SmPolyFunctionT m n p' t' b' pc' t)
    where t'paramClause (SmPolyFunctionT t'pc _) = t'pc

instance Case p t ct => Tree (SmTryT p t ct)
instance Case p t ct => WithCases p t ct (SmTryT p t ct)
    where cases (SmTryT _ cs _) = cs

instance Term t => Tree (SmTupleT t)
instance Term t => Tuple t (SmTupleT t)
    where args (SmTupleT as) = as

instance Term t => Tree (SmWhileT t)
instance Term t => WithBody t (SmWhileT t)
    where body (SmWhileT _ b) = b


instance ( ArgsType m
         , Term t
         ) => Tree (SmArgClauseT m t)
instance ( t ~ SmTerm
         , ArgsType m
         , Term t
         ) => SyntaxValuesClauses t (SmArgClauseT m t)
    where values (SmArgClauseT v _) = v
instance ( t ~ SmTerm
         , ArgsType m
         , Term t
         ) => ArgClause t (SmArgClauseT m t)
instance ( m ~ SmMod
         , t ~ SmTerm
         , ArgsType m
         , Term t
         ) => ArgClauseT m t (SmArgClauseT m t)
    where mod (SmArgClauseT _ m) = m


instance ( Mod m
         , Name n
         , Type' t'
         , Term t
         ) => Tree (SmParamT m n t' t)
instance ( Mod m
         , Name n
         , Type' t'
         , Term t
         ) => Member n (SmParamT m n t' t)
    where name (SmParamT _ n _ _) = n
instance ( Mod m
         , Name n
         , Type' t'
         , Term t
         ) => Param m n (SmParamT m n t' t)
    where mods' (SmParamT ms _ _ _) = ms
instance ( Mod m
         , Name n
         , Type' t'
         , Term t
         ) => WithDeclTpeOpt t' (SmParamT m n t' t)
    where decltpeOpt (SmParamT _ _ dt _) = dt
instance ( Mod m
         , Name n
         , Type' t'
         , Term t
         ) => ParamT m n (SmParamT m n t' t) t' t
    where defaultOpt (SmParamT _ _ _ d) = d


instance ( p ~ SmParamT m n t' t
         , ParamsType m
         , ParamT m n p t' t
         ) => Tree (SmParamClauseT m n p t' t)
instance ( p ~ SmParamT m n t' t
         , ParamsType m
         , ParamT m n p t' t
         ) => SyntaxValuesClauses p (SmParamClauseT m n p t' t)
    where values (SmParamClauseT vs _) = vs
instance ( p ~ SmParamT m n t' t
         , ParamsType m
         , ParamT m n p t' t
         ) => ParamClause m n p (SmParamClauseT m n p t' t)
instance ( p ~ SmParamT m n t' t
         , ParamsType m
         , ParamT m n p t' t
         ) => ParamClauseT m n p t' t (SmParamClauseT m n p t' t)
    where mod' (SmParamClauseT _ m) = m
