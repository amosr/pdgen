{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen where

import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Types as T

data PdNum = PdNum deriving (Eq,Show)
data PdBang = PdBang deriving (Eq,Show)
data PdAny = PdAny deriving (Eq,Show)
data PdSig = PdSig deriving (Eq,Show)

class Type p
instance Type PdNum
instance Type PdBang
instance Type PdAny
instance Type PdSig

class (Type o, Type i) => ConnectInto o i
instance (Type t) => ConnectInto t t
-- TODO XXX t NOT EQUAL to PdSig?
instance (Type t, T.TypeEq t PdSig ~ T.NotEq) => ConnectInto t PdAny
instance ConnectInto PdNum PdSig

data Type p => Inlet p = Inlet p C.Inlet deriving (Eq,Show)
data Type p => Outlet p = Outlet p C.Outlet deriving (Eq,Show)

type family TypeOfInlet p
type instance TypeOfInlet (Inlet i) = i

type InletsOfR l = T.MapR Inlet l
type OutletsOfR l = T.MapR Outlet l


data (List li, List lo) => Object li lo = Object {
	pref :: ObjectRef }
	pinlets :: li,
	poutlets :: lo }

infixl 5 @-
p @- n = Inlet (take (pinlets p) n) (p C.@- n)
infixl 5 @+
p @+ n = Outlet (take (poutlets p) n) (p C.@+ n)

infixl 3 @->
(@->) :: (ConnectInto o i) => Outlet o -> Inlet i -> C.Pd ()
(Outlet ot op) @-> (Inlet it ip) = op `C.into` ip

class (Type t, List l) => ConnectAll t l where
	connectAll :: Outlet t -> l -> C.Pd ()
instance (Type t) => ConnectAll t Nil where
	connectAll p i = return ()
instance (Type t, ConnectInto t (TypeOfPdInlet h), ConnectAll t r, h ~ Inlet hty) => ConnectAll t (Cons h r) where
	connectAll p (Cons h r) =
		p @-> h >> connectAll p r

