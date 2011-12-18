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

class ConstPdAny t where
	type ConstPdAnyR t
	constPdAny :: t -> ConstPdAnyR t
instance ConstPdAny t where
	type ConstPdAnyR t = PdAny
	constPdAny t = PdAny

mapPdAny :: (T.List l) => l -> T.MapR ConstPdAnyR l
mapPdAny ls = T.map constPdAny ls

pack ss = wrap (C.Object "pack" (listOfStringable ss))
	(mapPdAny ss) (T.l1 PdAny)
