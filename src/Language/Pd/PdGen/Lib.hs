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

-- | control inlet. not for signal.
inlet ty nm = wrap (C.Object "inlet" [nm]) (T.Nil) (T.l1 ty)
inletS nm = wrap (C.Object "inlet~" [nm]) (T.Nil) (T.l1 PdSig)
-- | control outlet. not for signal.
outlet ty nm = wrap (C.Object "outlet" [nm]) (T.l1 ty) (T.Nil)
outletS nm = wrap (C.Object "outlet~" [nm]) (T.l1 PdSig) (T.Nil)

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

num :: (Num n, Show n) => n -> String
num = show

binop1 nm f = wrap (C.Object nm [num f]) (T.l2 PdNum PdNum) (T.l1 PdNum)

add1 f = binop1 "+" f
sub1 f = binop1 "-" f
mul1 f = binop1 "*" f
div1 f = binop1 "/" f
pow1 f = binop1 "pow" f
max1 f = binop1 "max" f
min1 f = binop1 "min" f

unopF nm = wrap (C.Object nm []) (T.l1 PdNum) (T.l1 PdNum)
mtof = unopF "mtof"
ftom = unopF "ftom"
dbtorms = unopF "dbtorms"
rmstodb = unopF "rmstodb"

notein = wrap (C.Object "notein" []) T.Nil (T.l3 PdNum PdNum PdNum)

-- idea: (hot add1) makes all inlets hot with (t b f) etc

binopS nm = wrap (C.Object nm []) (T.l2 PdSig PdSig) (T.l1 PdSig)
binopS1 nm f = wrap (C.Object nm [num f]) (T.l2 PdSig PdNum) (T.l1 PdSig)

addS = binopS "+~"
addS1 f = binopS1 "+~" f
subS = binopS "-~"
subS1 f = binopS1 "-~" f
mulS = binopS "*~"
mulS1 f = binopS1 "*~" f
divS = binopS "/~"
divS1 f = binopS1 "/~" f
maxS = binopS "max~"
maxS1 f = binopS1 "max~" f
minS = binopS "min~"
minS1 f = binopS1 "min~" f

-- I'm not really sure about the second inlet...
lineS = wrap (C.Object "line~" []) (T.l2 PdAny PdNum) (T.l1 PdSig)

oscS = wrap (C.Object "osc~" []) (T.l2 PdSig PdNum) (T.l1 PdSig)
oscS1 f = wrap (C.Object "osc~" [num f]) (T.l2 PdNum PdNum) (T.l1 PdSig)

adcS2 = wrap (C.Object "adc~" []) T.Nil (T.l2 PdSig PdSig)
dacS2 = wrap (C.Object "dac~" []) (T.l2 PdSig PdSig) T.Nil

pipeF del = wrapObj "pipe" [num del] (T.l2 PdAny PdNum) (T.l1 PdNum)
delwriteS nm maxtime = wrapObj "delwrite~" [nm, num maxtime] (T.l1 PdSig) (T.Nil)
vdS nm = wrapObj "vd~" [nm] (T.l1 PdSig) (T.l1 PdSig)
phasorS = wrapObj "phasor~" [] (T.l2 PdSig PdNum) (T.l1 PdSig)
cosS = wrapObj "cos~" [] (T.l1 PdSig) (T.l1 PdSig)
sinS = wrapObj "sin~" [] (T.l1 PdSig) (T.l1 PdSig)

gt1 f = wrapObj ">" [num f] (T.l2 PdNum PdNum) (T.l1 PdNum)

svfS ty = wrapObj "svf~" [ty] (T.l4 PdSig PdSig PdSig PdSig) (T.l1 PdSig)

moses split = wrapObj "moses" [num split] (T.l2 PdNum PdNum) (T.l2 PdNum PdNum)


-- todo extra param voice stealing
poly :: Int -> Pd (Object (T.L1 PdAny) (T.L3 PdNum PdNum PdNum))
poly voices = wrap (C.Object "poly" [show voices, "1"])
	(T.l1 PdAny) (T.l3 PdNum PdNum PdNum)

{-
polysynth :: (T.Num voices, T.Range0 voices, voxR ~ T.Range0R voices,
		MapPdAny voxR, Stringable voxR, OutletsOf (MapPdAnyR voxR)) => voices ->
	Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig)) ->
	Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
-}

polysynth voices synth = do
	pk_1 <- LT.packT (T.l2 LT.TFloat LT.TFloat)
	p <- poly (T.intOfNum voices)
	pk_2 <- LT.packT (T.l3 LT.TFloat LT.TFloat LT.TFloat)
	Cn.manyToMany (outlets pk_1) (inlets p)
	Cn.manyToMany (outlets p) (inlets pk_2)
	r <- route (T.range0 voices)
	pk_2@+T.n0 @-> r@-T.n0
	busL <- addS
	busR <- addS
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
instance (PolySynth l) => PolySynth (T.Cons (Outlet PdAny) l) where
	polySynthR (T.Cons o p) synth collector = do
		unpk <- LT.unpack2F
		s <- synth
		o @-> unpk@-T.n0
		Cn.manyToMany (outlets unpk) (inlets s)
		Cn.manyToMany (outlets s) (inlets collector)
		
		polySynthR p synth collector

