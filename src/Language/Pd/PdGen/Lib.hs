{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen.Lib where

import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Connectors as Cn
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Lib.Trigger as LT


message :: String -> Pd (Object (T.L1 PdAny) (T.L1 PdAny))
message msg = wrap (C.Message msg) (T.l1 PdAny) (T.l1 PdAny)

number :: Pd (Object (T.L1 PdNum) (T.L1 PdNum))
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

route1 :: String -> Pd (Object (T.L2 PdAny PdAny) (T.L2 PdAny PdAny))
route1 s = wrap (C.Object "route" [s])
	(T.l2 PdAny PdAny) (T.l2 PdAny PdAny)

-- route :: (Stringable ps, MapPdAny ps) => ps -> Pd (Object (T.L1 PdAny) (MapPdAnyR ps))
route ps = wrap (C.Object "route" (listOfStringable ps))
	(T.l1 PdAny) (mapPdAny ps)

eq :: Float -> Pd (Object (T.L2 PdNum PdNum) (T.L1 PdNum))
eq f = wrap (C.Object "==" [show f]) (T.l2 PdNum PdNum) (T.l1 PdNum)

(+~) = wrap (C.Object "+~" []) (T.l2 PdSig PdSig) (T.l1 PdSig)

-- todo extra param voice stealing
poly :: Int -> Pd (Object (T.L1 PdAny) (T.L3 PdNum PdNum PdNum))
poly voices = wrap (C.Object "poly" [show voices])
	(T.l1 PdAny) (T.l3 PdNum PdNum PdNum)

{-
polysynth :: (T.Num voices, T.Range0 voices, voxR ~ T.Range0R voices,
		MapPdAny voxR, Stringable voxR, OutletsOf (MapPdAnyR voxR)) => voices ->
	Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig)) ->
	Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
-}

polysynth voices synth = do
	pk_1 <- pack (T.l2 PdNum PdNum)
	p <- poly (T.intOfNum voices)
	pk_2 <- pack (T.l3 PdNum PdNum PdNum)
	Cn.manyToMany (outlets pk_1) (inlets p)
	Cn.manyToMany (outlets p) (inlets pk_2)
	r <- route (T.range0 voices)
	busL <- (+~)
	busR <- (+~)
	polySynthR (outlets r) synth (Object (T.l2 (busL@-T.n0) (busR@-T.n0)) (T.Nil))
	return$ Object (T.l2 (pk_1@-T.n0) (pk_1@-T.n1)) (T.l2 (busL@+T.n0) (busR@+T.n0))

class PolySynth l where
	polySynthR ::
		l ->
		Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig)) ->
		(Object (T.L2 PdSig PdSig) T.Nil) ->
		Pd ()

instance PolySynth T.Nil where
	polySynthR _ _ _ = return () 
instance (PolySynth l) => PolySynth (T.Cons PdAny l) where
	polySynthR (T.Cons PdAny p) synth collector = do
		unpk <- LT.unpack (T.l2 LT.TFloat LT.TFloat)
		s <- synth
		Cn.manyToMany (outlets unpk) (inlets s)
		Cn.manyToMany (outlets s) (inlets collector)
		
		polySynthR p synth collector

