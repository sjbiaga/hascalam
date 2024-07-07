{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Show where

import Prelude hiding (seq)

import HaScalaM.Classes.Type
import HaScalaM.Types.Base
import HaScalaM.Types.Enums
import HaScalaM.Types.Pat
import HaScalaM.Types.Ref
import HaScalaM.Types.Stat
import HaScalaM.Types.Term
import HaScalaM.Types.Type
import HaScalaM.Types.Tilde
import Data.List (findIndices)


--------------------------------------------------------------------------- A --

instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , ac ~ SmArgClauseT m t
         , i ~ SmInit m n t' t ac
         ) => Show (SmAnnotM m n t' t ac i)
    where show (SmAnnotM i) = "Mod.Annot(" ++ show i ++ ")"

--------------------------------------------------------------------------- C --

instance ( p ~ SmPat
         , t ~ SmTerm
         ) => Show (SmCaseCT p t)
    where show (SmCaseC p c b) = "Case(" ++ show p ++ ", " ++ opt c ++ ", " ++ show b ++ ")"

instance t' ~ SmType' => Show (SmType'CaseCT t')
    where show (SmType'CaseT'C p b) = "TypeCase(" ++ show p ++ ", " ++ show b ++ ")"

instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , t ~ SmTerm
         , p ~ SmParamT m n t' t
         , pc ~ SmParamClauseT m n p t' t
         ) => Show (SmCtorPrimary m n p t' t pc)
    where show (SmCtorPrimary ms n pcs) = "Ctor.Primary(" ++ lst ms ++ ", " ++ show n ++ ", " ++ seq pcs ++ ")"

--------------------------------------------------------------------------- E --

instance Show SmEnumerator
    where show (ECaseGenerator (SmCaseGeneratorE p b)) = "Enumerator.CaseGenerator(" ++ show p ++ ", " ++ show b ++ ")"
          show (EGenerator (SmGeneratorE p b)) = "Enumerator.Generator(" ++ show p ++ ", " ++ show b ++ ")"
          show (EGuard (SmGuardE b)) = "Enumerator.Guard(" ++ show b ++ ")"
          show (EVal (SmValE p b)) = "Enumerator.Val(" ++ show p ++ ", " ++ show b ++ ")"

--------------------------------------------------------------------------- F --

instance t' ~ SmType' => Show (FuncParamClause' t')
    where show (FuncParamClause' vs) = "Type.FuncParamClause(" ++ lst vs ++ ")"

--------------------------------------------------------------------------- I --

instance ( n ~ SmName
         , t' ~ SmType'
         , ac ~ SmArgClauseT m t
         ) => Show (SmInit m n t' t ac)
    where show (SmInit tpe name acs) = "Init(" ++ show tpe ++ ", " ++ show name ++ ", " ++ seq acs ++ ")"

instance Show SmImportee
    where show (SmGivenI tpe) = "Importee.Given(" ++ show tpe ++ ")"
          show SmGivenAllI = "Importee.GivenAll()"
          show (SmNameI n) = "Importee.Name(" ++ show n ++ ")"
          show (SmRenameI m n) = "Importee.Rename(" ++ show m ++ ", " ++ show n ++ ")"
          show (SmUnimportI n) = "Importee.Unimport(" ++ show n ++ ")"
          show SmWildcardI = "Importee.Wildcard()"

instance Show (SmImporter r i)
    where show (SmImporter r is) = "Importer(" ++ show r ++ ", " ++ lst is ++ ")"

--------------------------------------------------------------------------- L --

instance Show SmLit
    where show (SmBooleanL v) = "Lit.Boolean (" ++ show v ++ ")"
          show (SmByteL v) = "Lit.Byte(" ++ show v ++ ")"
          show (SmCharL v) = "Lit.Char(" ++ show v ++ ")"
          show (SmDoubleL v) = "Lit.Double(" ++ show v ++ ")"
          show (SmFloatL v) = "Lit.Float(" ++ show v ++ ")"
          show (SmIntL v) = "Lit.Int(" ++ show v ++ ")"
          show (SmLongL v) = "Lit.Long(" ++ show v ++ ")"
          show  SmNullL = "Lit.Null()"
          show (SmShortL v) = "Lit.Short(" ++ show v ++ ")"
          show (SmStringL v) = "Lit.String(" ++ show v ++ ")"
          show (SmSymbolL v) = "Lit.Symbol(" ++ show v ++ ")"
          show SmUnitL = "Lit.Unit()"

--------------------------------------------------------------------------- M --

instance ( i ~ SmInit m n t' t ac
         , r ~ SmRef_
         ) => Show SmMod
    where show (MAnnot (SmAnnotM i)) = "Mod.Annot(" ++ show i ++ ")"
          show (MMod SmAbstractM) = "Mod.Abstract()"
          show (MMod SmCaseM) = "Mod.Case()"
          show (MMod SmCovariantM) = "Mod.Covariant()"
          show (MMod SmContravariantM) = "Mod.Contravariant()"
          show (MMod SmErasedM) = "Mod.Erased()"
          show (MMod SmFinalM) = "Mod.Final()"
          show (MMod SmImplicitM) = "Mod.Implicit()"
          show (MMod SmInfixM) = "Mod.Infix()"
          show (MMod SmInlineM) = "Mod.Inline()"
          show (MMod SmLazyM) = "Mod.Lazy()"
          show (MMod SmOpaqueM) = "Mod.Opaque()"
          show (MMod SmOpenM) = "Mod.Open()"
          show (MMod SmOverrideM) = "Mod.Override()"
          show (MAccess (SmPrivateM r)) = "Mod.Private(" ++ show r ++ ")"
          show (MAccess (SmProtectedM r)) = "Mod.Protected(" ++ show r ++ ")"
          show (MMod SmSealedM) = "Mod.Sealed()"
          show (MMod SmSuperM) = "Mod.Super()"
          show (MMod SmTransparentM) = "Mod.Transparent()"
          show (MMod SmUsingM) = "Mod.Using()"
          show (MMod SmValParamM) = "Mod.ValParam()"
          show (MMod SmVarParamM) = "Mod.VarParam()"

--------------------------------------------------------------------------- N --

instance Show SmName
    where show (NName SmAnonymousN) = "Name.Anonymous()"
          show (NName (SmIndeterminateN v)) = "Name.Indeterminate(" ++ show v ++ ")"
          show (NName SmPlaceholderN) = "Name.Placeholder()"
          show (NName SmThisN) = "Name.This()"
          show (NAnonymous SmAnonymousRT) = "Term.Anonymous()"
          show (NTName n) = show n
          show (NT'Name n') = show n'


instance Show SmNameT
    where show (SmNameT v) = "Term.Name(" ++ show v ++ ")"


instance Show SmNameT'
    where show (SmNameT' v) = "Type.Name(" ++ show v ++ ")"

--------------------------------------------------------------------------- P --

instance Show (SmParamClauseGroup m n p p' t' b' t pc pc')
    where show (SmParamClauseGroup t'pc pcs) = "Member.ParamClauseGroup" ++ show t'pc ++ ", " ++ lst pcs ++ ")"

instance Show SmPat
    where show (PLit (SmBooleanL v)) = "Lit.Boolean(" ++ show v ++ ")"
          show (PLit (SmByteL v)) = "Lit.Byte(" ++ show v ++ ")"
          show (PLit (SmCharL v)) = "Lit.Char(" ++ show v ++ ")"
          show (PLit (SmDoubleL v)) = "Lit.Double(" ++ show v ++ ")"
          show (PLit (SmFloatL v)) = "Lit.Float(" ++ show v ++ ")"
          show (PLit (SmIntL v)) = "Lit.Int(" ++ show v ++ ")"
          show (PLit (SmLongL v)) = "Lit.Long(" ++ show v ++ ")"
          show (PLit SmNullL) = "Lit.Null()"
          show (PLit (SmShortL v)) = "Lit.Short(" ++ show v ++ ")"
          show (PLit (SmStringL v)) = "Lit.String(" ++ show v ++ ")"
          show (PLit (SmSymbolL v)) = "Lit.Symbol(" ++ show v ++ ")"
          show (PLit SmUnitL) = "Lit.Unit()"
          show (PPat (SmAlternativeP lhs rhs)) = "Pat.Alternative(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (PPat (SmBindP lhs rhs)) = "Pat.Bind(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (PExtract (SmExtractP f ac)) = "Pat.Extract(" ++ show f ++ ", " ++ show ac ++ ")"
          show (PExtractInfix (SmExtractInfixP lhs op ac)) = "Pat.ExtractInfix(" ++ show lhs ++ ", " ++ show op ++ ", " ++ show ac ++ ")"
          show (PPat (SmGivenP tpe)) = "Pat.Given(" ++ show tpe ++ ")"
          show (PPat (SmInterpolateP n ls as)) = "Pat.Interpolate(" ++ show n ++ ", " ++ lst ls ++ ", " ++ lst as ++ ")"
          show (PMacro (SmMacroP b)) = "Pat.Macro(" ++ show b ++ ")"
          show (PPat (SmRepeatedP n)) = "Pat.Repeated(" ++ show n ++ ")"
          show (PPat (SmNameP n)) = show n
          show (PPat (SmSelectP (SmSelectRT q n))) = "Term.Select(" ++ show q ++ ", " ++ show n ++ ")"
          show (PPat SmSeqWildcardP) = "Pat.SeqWildcard()"
          show (PTuple (SmTupleP as)) = "Pat.Tuple(" ++ lst as ++ ")"
          show (PPat (SmTypedP lhs rhs)) = "Pat.Typed(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (PVar (SmVarP n)) = "Pat.Var(" ++ show n ++ ")"
          show (PPat SmWildcardP) = "Pat.Wildcard()"
          show (PPat (SmXmlP ls as)) = "Pat.Xml(" ++ lst ls ++ ", " ++ lst as ++ ")"


instance Show (SmArgClauseP p)
    where show (SmArgClauseP vs) = "Pat.ArgClause(" ++ lst vs ++ ")"


instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , t ~ SmTerm
         ) => Show (SmParamT m n t' t)
    where show (SmParamT ms n dt d) = "Term.Param(" ++ lst ms ++ ", " ++ show n ++ ", " ++ opt dt ++ ", " ++ opt d ++ ")"


instance ( m ~ SmMod
         , t' ~ SmType'
         , n ~ SmName
         , t ~ SmTerm
         , p ~ SmParamT m n t' t
         ) => Show (SmParamClauseT m n p t' t)
    where show (SmParamClauseT ps m) = "Term.ParamClause(" ++ lst ps ++ ", " ++ opt m ++ ")"


instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , b' ~ SmBounds' t'
         , p' ~ SmParamT' m n t' b'
         , pc' ~ SmParamClauseT' m n p' t' b'
         ) => Show (SmParamT' m n t' b')
    where show (SmParamT' ms n t'pc tb vbs cbs) = "Type.Param(" ++ lst ms ++ ", " ++ show n ++ ", " ++ show t'pc ++ ", " ++ show tb ++ ", " ++ lst vbs ++ ", " ++ lst cbs ++ ")"


instance ( m ~ SmMod
         , n ~ SmName
         , t' ~ SmType'
         , b' ~ SmBounds' t'
         , p' ~ SmParamT' m n t' b'
         ) => Show (SmParamClauseT' m n p' t' b')
     where show (SmParamClauseT' p's) = "Type.ParamClause(" ++ lst p's ++ ")"

--------------------------------------------------------------------------- R --

instance Show SmRef_
    where show (RImportee i) = show i
          show (RInit i) = show i
          show (RName n) = show n
          show (R_TRef r) = show r
          show (R_T'Ref r) = show r


instance Show SmRef
    where show (RTAnonymous SmAnonymousRT) = "Term.Anonymous()"
          show (RTRef (SmApplyUnaryRT n t)) = "Term.ApplyUnary(" ++ show n ++ ", " ++ show t ++ ")"
          show (RTName n) = show n
          show (RTSelect (SmSelectRT q n)) = "Term.Select(" ++ show q ++ ", " ++ show n ++ ")"
          show (RTRef (SmSuperRT m n)) = "Term.Super(" ++ show m ++ ", " ++ show n ++ ")"
          show (RTRef (SmThisRT n)) = "Term.This(" ++ show n ++ ")"


instance Show SmRef'
    where show (RT'Name n) = show n
          show (RT'Ref (SmProjectRT' q n)) = "Type.Project(" ++ show q ++ ", " ++ show n ++ ")"
          show (RT'Ref (SmSelectRT' q n)) = "Type.Select(" ++ show q ++ ", " ++ show n ++ ")"
          show (RT'Ref (SmSingletonRT' r)) = "Type.Singleton(" ++ show r ++ ")"

--------------------------------------------------------------------------- S --

instance ( n ~ SmName
         , t' ~ SmType'
         ) => Show (SmSelf n t')
    where show (SmSelf n dt) = "Self(" ++ show n ++ ", " ++ opt dt ++ ")"

instance Show SmStat
    where
    show (S'' s) = s
    show (SCtorSecondary (SmCtorSecondaryS ms n pcs i ss)) = "Ctor.Secondary(" ++ lst ms ++ ", " ++ show n ++ ", " ++ seq pcs ++ ", " ++ show ss ++ ")"
    show (SDef' (SmDef'S ms tn gs dt)) = "Decl.Def(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ lst gs ++ ", " ++ show dt ++ ")"
    show (SGiven' (SmGiven'S ms tn g dt)) = "Decl.Given(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ opt g ++ ", " ++ show dt ++ ")"
    show (SType' (SmType'S ms t'n t'pc b')) = "Decl.Type(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show b' ++ ")"
    show (SVal' (SmVal'S ms ps dt)) = "Decl.Val(" ++ lst ms ++ ", " ++ lst ps ++ ", " ++ show dt ++ ")"
    show (SVar' (SmVar'S ms ps dt)) = "Decl.Var(" ++ lst ms ++ ", " ++ lst ps ++ ", " ++ show dt ++ ")"
    show (SClass (SmClassS ms t'n t'pc c e)) = "Defn.Class(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show c ++ ", " ++ show e ++ ")"
    show (SDef (SmDefS ms tn gs dt b)) = "Defn.Def(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ lst gs ++ ", " ++ opt dt ++ ", " ++ show b ++ ")"
    show (SEnum (SmEnumS ms t'n t'pc c e)) = "Defn.Enum(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show c ++ ", " ++ show e ++ ")"
    show (SEnumCase (SmEnumCaseS ms tn t'pc c is)) = "Defn.EnumCase(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ show t'pc ++ ", " ++ show c ++ ", " ++ lst is ++ ")"
    show (SExtensionGroup (SmExtensionGroupS g b)) = "Defn.ExtensionGroup(" ++ opt g ++ ", " ++ show b ++ ")"
    show (SGiven (SmGivenS ms n g e)) = "Defn.Given(" ++ lst ms ++ ", " ++ show n ++ ", " ++ opt g ++ ", " ++ show e ++ ")"
    show (SGivenAlias (SmGivenAliasS ms n g dt b)) = "Defn.GivenAlias(" ++ lst ms ++ ", " ++ show n ++ ", " ++ opt g ++ ", " ++ show dt ++ ", " ++ show b ++ ")"
    show (SMacro (SmMacroS ms tn gs dt b)) = "Defn.Macro(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ lst gs ++ ", " ++ opt dt ++ ", " ++ show b ++ ")"
    show (SObject (SmObjectS ms tn e)) = "Defn.Object(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ show e ++ ")"
    show (SRepeatedEnumCase  (SmRepeatedEnumCase ms tns)) = "Defn.RepeatedEnumCase(" ++ lst ms ++ ", " ++ lst tns ++ ")"
    show (STrait (SmTraitS ms t'n t'pc c e)) = "Defn.Trait(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show c ++ ", " ++ show e ++ ")"
    show (SType (SmTypeS ms t'n t'pc b b')) = "Defn.Type(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show b ++ ", " ++ show b' ++ ")"
    show (SVal (SmValS ms ps dt rhs)) = "Defn.Val(" ++ lst ms ++ ", " ++ lst ps ++ ", " ++ show dt ++ ", " ++ show rhs ++ ")"
    show (SVar (SmVarS ms ps dt rhs)) = "Defn.Var(" ++ lst ms ++ ", " ++ lst ps ++ ", " ++ show dt ++ ", " ++ show rhs ++ ")"
    show (SImpExp (SmExportS is)) = "Export(" ++ lst is ++ ")"
    show (SImpExp (SmImportS is)) = "Import(" ++ lst is ++ ")"
    show (SPkg (SmPkgS r ss)) = "Pkg(" ++ show r ++ ", " ++ lst ss ++ ")"
    show (SPkgObject (SmPkgObjectS ms tn e)) = "Pkg.Object(" ++ lst ms ++ ", " ++ show tn ++ ", " ++ show e ++ ")"
    show (STerm t) = show t


instance Show (SmSource s)
    where show (SmSource ss) = "Source(" ++ lst ss ++ ")"


instance Show Symbol
    where show (Symbol v) = "Symbol(" ++ show v ++ ")"

--------------------------------------------------------------------------- T --

instance Show (SmTemplate m n t' t ac i p s)
    where show (SmTemplate es is s ss t's) = "Template(" ++ lst es ++ ", " ++ lst is ++ ", " ++ show s ++ ", " ++ lst ss ++ ", " ++ lst t's ++ ")"


instance Show SmTerm
    where
    show (T'' s) = s
    show (TLit (SmBooleanL v)) = "Lit.Boolean(" ++ show v ++ ")"
    show (TLit (SmByteL v)) = "Lit.Byte(" ++ show v ++ ")"
    show (TLit (SmCharL v)) = "Lit.Char(" ++ show v ++ ")"
    show (TLit (SmDoubleL v)) = "Lit.Double(" ++ show v ++ ")"
    show (TLit (SmFloatL v)) = "Lit.Float(" ++ show v ++ ")"
    show (TLit (SmIntL v)) = "Lit.Int(" ++ show v ++ ")"
    show (TLit (SmLongL v)) = "Lit.Long(" ++ show v ++ ")"
    show (TLit SmNullL) = "Lit.Null()"
    show (TLit (SmShortL v)) = "Lit.Short(" ++ show v ++ ")"
    show (TLit (SmStringL v)) = "Lit.String(" ++ show v ++ ")"
    show (TLit (SmSymbolL v)) = "Lit.Symbol(" ++ show v ++ ")"
    show (TLit SmUnitL) = "Lit.Unit()"
    show (TTerm (SmAnnotateT e as)) = "Term.Annotate(" ++ show e ++ ", " ++ lst as ++ ")"
    show (TTerm (SmAnonymousFunctionT b)) = "Term.AnonymousFunction(" ++ show b ++ ")"
    show (TApply (SmApplyT f ac)) = "Term.Apply(" ++ show f ++ ", " ++ show ac ++ ")"
    show (TApply (SmApplyUsingT f ac)) = "Term.ApplyUsing(" ++ show f ++ ", " ++ show ac ++ ")"
    show (TApplyInfix (SmApplyInfixT lhs op t'ac ac)) = "Term.ApplyInfix(" ++ show lhs ++ ", " ++ show op ++ ", " ++ show t'ac ++ ", " ++ show ac ++ ")"
    show (TApplyType' (SmApplyType'T f t'ac)) = "Term.ApplyType(" ++ show f ++ ", " ++ show t'ac ++ ")"
    show (TTerm (SmAscribeT expr tpe)) = "Term.Ascribe(" ++ show expr ++ ", " ++ show tpe ++ ")"
    show (TAssign (SmAssignT lhs rhs)) = "Term.Assign(" ++ show lhs ++ ", " ++ show rhs ++ ")"
    show (TBlock (SmBlockT ss)) = "Term.Block(" ++ lst ss ++ ")"
    show (TContextFunction (SmContextFunctionT pc b)) = "Term.ContextFunction(" ++ show pc ++ ", " ++ show b ++ ")"
    show (TDo (SmDoT b e)) = "Term.Do(" ++ show b ++ ", " ++ show e ++ ")"
    show (TTerm (SmEndMarkerT n)) = "Term.EndMarker(" ++ show n ++ ")"
    show (TTerm (SmEtaT e)) = "Term.Eta(" ++ show e ++ ")"
    show (TFor (SmForT es b)) = "Term.For(" ++ lst es ++ ", " ++ show b ++ ")"
    show (TForYield (SmForYieldT es b)) = "Term.ForYield(" ++ lst es ++ ", " ++ show b ++ ")"
    show (TFunction (SmFunctionT pc b)) = "Term.Function(" ++ show pc ++ ", " ++ show b ++ ")"
    show (TIf (SmIfT c t e ms)) = "Term.If(" ++ show c ++ ", " ++ show t ++ ", " ++ show e ++ ", " ++ lst ms ++ ")"
    show (TTerm (SmInterpolateT n ls as)) = "Term.Interpolate(" ++ show n ++ ", " ++ lst ls ++ ", " ++ lst as ++ ")"
    show (TMatch (SmMatchT e cs)) = "Term.Match(" ++ show e ++ ", " ++ lst cs ++ ")"
    show (TTerm (SmNewT i)) = "Term.New(" ++ show i ++ ")"
    show (TNewAnonymous (SmNewAnonymousT e)) = "Term.NewAnonymous(" ++ show e ++ ")"
    show (TPartialFunction (SmPartialFunctionT cs)) = "Term.PartialFunction(" ++ lst cs ++ ")"
    show (TTerm SmPlaceholder) = "Term.Placeholder()"
    show (TPolyFunction (SmPolyFunctionT t'pc b)) = "Term.PolyFunction(" ++ show t'pc ++ ", " ++ show b ++ ")"
    show (TTerm (SmQuotedMacroExprT b)) = "Term.QuotedMacroExpr(" ++ show b ++ ")"
    show (TTerm (SmQuotedMacroType'T tpe)) = "Term.QuotedMacroType(" ++ show tpe ++ ")"
    show (TTerm (SmRepeatedT e)) = "Term.Repeated(" ++ show e ++ ")"
    show (TTerm (SmReturnT e)) = "Term.Return(" ++ show e ++ ")"
    show (TTerm (SmSplicedMacroExprT b)) = "Term.SplicedMacroExpr(" ++ show b ++ ")"
    show (TTerm (SmSplicedMacroPatT p)) = "Term.SplicedMacroPat(" ++ show p ++ ")"
    show (TTerm (SmThrowT e)) = "Term.Throw(" ++ show e ++ ")"
    show (TTry (SmTryT e cs f)) = "Term.Try(" ++ show e ++ ", " ++ lst cs ++ ", " ++ opt f ++ ")"
    show (TTerm (SmTryWithHandlerT e c f)) = "Term.TryWithHandler(" ++ show e ++ ", " ++ show c ++ ", " ++ show f ++ ")"
    show (TTuple (SmTupleT as)) = "Term.Tuple(" ++ lst as ++ ")"
    show (TWhile (SmWhileT e b)) = "Term.While(" ++ show e ++ ", " ++ show b ++ ")"
    show (TTerm (SmXmlT ls as)) = "Term.Xml(" ++ lst ls ++ ", " ++ lst as ++ ")"
    show (TRef (RTAnonymous SmAnonymousRT)) = "Term.Anonymous()"
    show (TRef (RTRef (SmApplyUnaryRT n t))) = "Term.ApplyUnary(" ++ show n ++ ", " ++ show t ++ ")"
    show (TRef (RTName n)) = show n
    show (TRef (RTSelect (SmSelectRT q n))) = "Term.Select(" ++ show q ++ ", " ++ show n ++ ")"
    show (TRef (RTRef (SmSuperRT m n))) = "Term.Super(" ++ show m ++ ", " ++ show n ++ ")"
    show (TRef (RTRef (SmThisRT n))) = "Term.This(" ++ show n ++ ")"

instance ( m ~ SmMod
         , t ~ SmTerm
         ) => Show (SmArgClauseT m t)
    where show (SmArgClauseT vs mod) = "Term.ArgClause(" ++ lst vs ++ ", " ++ opt mod ++ ")"


instance Show SmType'
    where show (T'Lit (SmBooleanL v)) = "Lit.Boolean(" ++ show v ++ ")"
          show (T'Lit (SmByteL v)) = "Lit.Byte(" ++ show v ++ ")"
          show (T'Lit (SmCharL v)) = "Lit.Char(" ++ show v ++ ")"
          show (T'Lit (SmDoubleL v)) = "Lit.Double(" ++ show v ++ ")"
          show (T'Lit (SmFloatL v)) = "Lit.Float(" ++ show v ++ ")"
          show (T'Lit (SmIntL v)) = "Lit.Int(" ++ show v ++ ")"
          show (T'Lit (SmLongL v)) = "Lit.Long(" ++ show v ++ ")"
          show (T'Lit SmNullL) = "Lit.Null()"
          show (T'Lit (SmShortL v)) = "Lit.Short(" ++ show v ++ ")"
          show (T'Lit (SmStringL v)) = "Lit.String(" ++ show v ++ ")"
          show (T'Lit (SmSymbolL v)) = "Lit.Symbol(" ++ show v ++ ")"
          show (T'Lit SmUnitL) = "Lit.Unit()"
          show (T'Type' (SmAndT' lhs rhs)) = "Type.And(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (T'Type' (SmAnnotateT' tpe as)) = "Type.Annotate(" ++ show tpe ++ ", " ++ lst as ++ ")"
          show (T'Type' (SmAnonymousLambdaT' v)) = "Type.AnonymousLambda(" ++ show v ++ ")"
          show (T'Type' SmAnonymousNameT') = "Type.AnonymousName()"
          show (T'Type' (SmAnonymousParamT' m)) = "Type.AnonymousParam(" ++ opt m ++ ")"
          show (T'Apply (SmApplyT' tpe ac)) = "Type.Apply(" ++ show tpe ++ ", " ++ show ac ++ ")"
          show (T'ApplyInfix (SmApplyInfixT' lhs op rhs)) = "Type.ApplyInfix(" ++ show lhs ++ ", " ++ show op ++ ", " ++ show rhs ++ ")"
          show (T'Type' (SmBlockT' t'ds tpe)) = "Type.Block(" ++ lst t'ds ++ ", " ++ show tpe ++ ")"
          show (T'Type' (SmByNameT' tpe)) = "Type.ByName(" ++ show tpe ++ ")"
          show (T'ContextFunction (SmContextFunctionT' pc res)) = "Type.ContextFunction(" ++ show pc ++ ", " ++ show res ++ ")"
          show (T'Existential (SmExistentialT' tpe ss)) = "Type.Existential(" ++ show tpe ++ ", " ++ lst ss ++ ")"
          show (T'Function (SmFunctionT' pc res)) = "Type.Function(" ++ show pc ++ ", " ++ show res ++ ")"
          show (T'Type' (SmImplicitFunctionT' ps res)) = "Type.ImplicitFunction(" ++ lst ps ++ ", " ++ show ps ++ ")"
          show (T'Lambda (SmLambdaT' t'pc tpe)) = "Type.Lambda(" ++ show t'pc ++ ", " ++ show tpe ++ ")"
          show (T'Macro (SmMacroT' b)) = "Type.Macro(" ++ show b ++ ")"
          show (T'Match (SmMatchT' tpe cs)) = "Type.Match(" ++ show tpe ++ ", " ++ lst cs ++ ")"
          show (T'Type' (SmMethodT' pcs tpe)) = "Type.Method(" ++ seq pcs ++ ", " ++ show tpe ++ ")"
          show (T'Type' (SmOrT' lhs rhs)) = "Type.Or(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (T'PolyFunction (SmPolyFunctionT' t'pc b)) = "Type.PolyFunction(" ++ show t'pc ++ ", " ++ show b ++ ")"
          show (T'Refine (SmRefineT' tpe ss)) = "Type.Refine(" ++ show tpe ++ ", " ++ lst ss ++ ")"
          show (T'Type' (SmRepeatedT' tpe)) = "Type.Repeated(" ++ show tpe ++ ")"
          show (T'Type' SmPatWildcardT') = "Type.PatWildcard()"
          show (T'Tuple (SmTupleT' as)) = "Type.Tuple(" ++ lst as ++ ")"
          show (T'Var (SmVarT' v)) = "Type.Var(" ++ show v ++ ")"
          show (T'Type' (SmWithT' lhs rhs)) = "Type.With(" ++ show lhs ++ ", " ++ show rhs ++ ")"
          show (T'Type' (SmWildcardT' b')) = "Type.Wildcard(" ++ show b' ++ ")"
          show (T'Ref (RT'Name n')) = show n'
          show (T'Ref (RT'Ref (SmProjectRT' q n))) = "Type.Project(" ++ show q ++ ", " ++ show n ++ ")"
          show (T'Ref (RT'Ref (SmSelectRT' q n))) = "Type.Select(" ++ show q ++ ", " ++ show n ++ ")"
          show (T'Ref (RT'Ref (SmSingletonRT' r))) = "Type.Singleton(" ++ show r ++ ")"


instance t' ~ SmType' => Show (SmArgClauseT' t')
    where show (SmArgClauseT' vs) = "Type.ArgClause(" ++ lst vs ++ ")"


instance t' ~ SmType' => Show (SmBounds' t')
    where show (SmBounds' lo hi) = "Type.Bounds(" ++ opt lo ++ ", " ++ opt hi ++ ")"


instance ( m ~ SmMod
         , n ~ SmName
         , t'n ~ SmNameT'
         , p' ~ SmParamT' m n t' b'
         , t' ~ SmType'
         , b' ~ SmBounds' t'
         , pc' ~ SmParamClauseT' m n p' t' b'
         ) => Show (SmType'Def m n t'n p' t' b' pc')
    where show (T'DType' (SmType'S ms t'n t'pc b')) = "Decl.Type(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show b' ++ ")"
          show (T'DType (SmTypeS ms t'n t'pc b b')) = "Defn.Type(" ++ lst ms ++ ", " ++ show t'n ++ ", " ++ show t'pc ++ ", " ++ show b ++ ", " ++ show b' ++ ")"

--------------------------------------------------------------------------------

csv :: Show t => [t] -> String
csv ls = concat [ show v ++ c | (v,i) <- ps, let c = if i + 1 < length ls then ", " else "" ]
    where ps = zip ls (findIndices (const True) ls)

lst :: Show t => [t] -> String
lst [] = "Nil"
lst ls = "List(" ++ csv ls ++ ")"

seq :: Show t => [t] -> String
seq [] = "Seq()"
seq ls = "Seq(" ++ csv ls ++ ")"

opt :: Show t => Maybe t -> String
opt (Just v) = "Some(" ++ show v ++ ")"
opt _ = "None"
