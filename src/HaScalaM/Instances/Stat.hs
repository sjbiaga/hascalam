{-# LANGUAGE TypeFamilies #-}
module HaScalaM.Instances.Stat where

import HaScalaM.Classes
import HaScalaM.Classes.Base
import HaScalaM.Classes.Ref
import HaScalaM.Classes.Stat
import HaScalaM.Classes.Term
import HaScalaM.Types.Base
import HaScalaM.Types.Ref
import HaScalaM.Types.Stat
import HaScalaM.Types.Tilde
import HaScalaM.Instances.Ref


-- I ---------------------------------------------------------------------------

instance Tree SmImportee
instance Importee SmImportee

instance ( RefT r
         , Importee i
         ) => Tree (SmImporter r i)
instance ( r ~ SmRef
         , i ~ SmImportee
         , RefT r
         , Importee i
         ) => Importer r i (SmImporter r i)
    where ref       (SmImporter r _) = r
          importees (SmImporter _ is) = is

instance Importer r i t => Tree (SmImportExportStatS r i t)
instance Importer r i t => ImportExportStat r i t (SmImportExportStatS r i t)
    where importers (SmImportS is) = is
          importers (SmExportS is) = is

-- P ---------------------------------------------------------------------------

instance Stat s => Tree (SmPkgS SmRef s)
instance Stat s => Member SmNameT (SmPkgS SmRef s)
    where name (SmPkgS (RTName n) _) = n
          name (SmPkgS (RTSelect (SmSelectRT _ n)) _) = n
instance Stat s => MemberT SmNameT (SmPkgS SmRef s)

instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => Tree (SmPkgObjectS m n p tn t' t ac i s e)
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => WithMods m (SmPkgObjectS m n p tn t' t ac i s e)
    where mods (SmPkgObjectS ms _ _) = ms
instance ( NameT tn
         , Template m n t' t ac i p s e
         ) => WithTemplate m n t' t ac i p s e (SmPkgObjectS m n p tn t' t ac i s e)
    where templ (SmPkgObjectS _ _ t) = t

-- S ---------------------------------------------------------------------------

instance Tree SmStat
instance Stat SmStat
