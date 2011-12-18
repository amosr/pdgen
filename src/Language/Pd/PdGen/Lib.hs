{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen.Lib where

import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Types as T

message :: String -> C.Pd (Object (T.L1 PdAny) (T.L1 PdAny))
message msg = wrap (C.Message msg) (T.l1 PdAny) (T.l1 PdAny)
number = wrap C.FloatAtom (T.l1 PdNum) (T.l1 PdNum)
symbol = wrap C.SymbolAtom (T.l1 PdAny) (T.l1 PdAny)
print msg = wrap (C.Object "print" [msg]) (T.l1 PdAny) (T.Nil)

inlet ty nm = wrap (C.Object "inlet" [nm]) (T.Nil) (T.l1 ty)
outlet ty nm = wrap (C.Object "outlet" [nm]) (T.l1 ty) (T.Nil)

swap = wrap (C.Object "swap" []) (T.l2 PdNum PdNum) (T.l2 PdNum PdNum)

class Stringable p where
	listOfStringable :: p -> [String]
instance Stringable T.Nil where
	listOfStringable _ = []
instance (Show h, Stringable t) => Stringable (T.Cons h t) where
	listOfStringable (T.Cons h t) = show h : listOfStringable t

-- can't figure out how to use T.Map with partially applied type synonyms..
-- so MapPdAny = Map (const PdAny)
class (T.List l, ListOfType (MapPdAnyR l)) => MapPdAny l where
	type MapPdAnyR l
	mapPdAny :: l -> MapPdAnyR l
instance MapPdAny T.Nil where
	type MapPdAnyR T.Nil = T.Nil
	mapPdAny _ = T.Nil
instance (MapPdAny t) => MapPdAny (T.Cons h t) where
	type MapPdAnyR (T.Cons h t) = T.Cons PdAny (MapPdAnyR t)
	mapPdAny (T.Cons _ t) = T.Cons PdAny (mapPdAny t)

pack ss = wrap (C.Object "pack" (listOfStringable ss))
	(mapPdAny ss) (T.l1 PdAny)

route1 :: String -> C.Pd (Object (T.L2 PdAny PdAny) (T.L2 PdAny PdAny))
route1 s = wrap (C.Object "route" [s])
	(T.l2 PdAny PdAny) (T.l2 PdAny PdAny)

route :: (Stringable ps, MapPdAny ps) => ps -> C.Pd (Object (T.L1 PdAny) (MapPdAnyR ps))
route ps = wrap (C.Object "route" (listOfStringable ps))
	(T.l1 PdAny) (mapPdAny ps)

eq :: Float -> C.Pd (Object (T.L2 PdNum PdNum) (T.L1 PdNum))
eq f = wrap (C.Object "==" [show f]) (T.l2 PdNum PdNum) (T.l1 PdNum)

