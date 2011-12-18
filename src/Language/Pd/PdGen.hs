{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen where

import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Types as T

data PdNum = PdNum deriving (Eq,Show)
data PdBang = PdBang deriving (Eq,Show)
data PdSymbol = PdSymbol deriving (Eq,Show)
data PdAny = PdAny deriving (Eq,Show)
data PdSig = PdSig deriving (Eq,Show)

class Type p
instance Type PdNum
instance Type PdBang
instance Type PdSymbol
instance Type PdAny
instance Type PdSig

class (Type o, Type i) => ConnectInto o i
instance (Type t) => ConnectInto t t
-- TODO XXX t NOT EQUAL to PdSig?
instance (Type t) => ConnectInto t PdAny
instance ConnectInto PdNum PdSig
instance ConnectInto PdAny PdAny

data Type p => Inlet p = Inlet p C.Inlet deriving (Eq,Show)
data Type p => Outlet p = Outlet p C.Outlet deriving (Eq,Show)

type family TypeOfInlet p
type instance TypeOfInlet (Inlet i) = i

class ListOfType l
instance ListOfType T.Nil
instance (Type h, ListOfType t) => ListOfType (T.Cons h t)

data (ListOfType li, ListOfType lo) => Object li lo = Object {
	pref :: C.ObjectRef,
	pinlets :: li,
	poutlets :: lo }

infixl 5 @-
p @- n = Inlet (T.take (pinlets p) n) (pref p C.@- T.intOfNum n)
infixl 5 @+
p @+ n = Outlet (T.take (poutlets p) n) (pref p C.@+ T.intOfNum n)

infixl 3 @->
(@->) :: (ConnectInto o i) => Outlet o -> Inlet i -> C.Pd ()
(Outlet ot op) @-> (Inlet it ip) = op `C.into` ip

infixl 3 `forceInto`
forceInto :: (Type o,Type i) => Outlet o -> Inlet i -> C.Pd ()
forceInto (Outlet ot op) (Inlet it ip) = op `C.into` ip

class (Type t, T.List l) => ConnectAll t l where
	connectAll :: Outlet t -> l -> C.Pd ()
instance (Type t) => ConnectAll t T.Nil where
	connectAll p i = return ()
instance (Type t, ConnectInto t (TypeOfInlet h), ConnectAll t r, h ~ Inlet hty) => ConnectAll t (T.Cons h r) where
	connectAll p (T.Cons h r) =
		p @-> h >> connectAll p r

wrap obj ins outs = do
	ref <- C.insertObject obj
	return$ Object ref ins outs
