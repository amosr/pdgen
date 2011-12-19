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
	inlets :: T.MapR Inlet li,
	outlets :: T.MapR Outlet lo }

infixl 5 @-
p @- n = T.take (inlets p) n
infixl 5 @+
p @+ n = T.take (outlets p) n

type Pd a = C.Pd a

infixl 3 @->
(@->) :: (ConnectInto o i) => Outlet o -> Inlet i -> Pd ()
(Outlet ot op) @-> (Inlet it ip) = op `C.into` ip

infixl 3 `forceInto`
forceInto :: (Type o,Type i) => Outlet o -> Inlet i -> Pd ()
forceInto (Outlet ot op) (Inlet it ip) = op `C.into` ip

class (ListOfType ts) => InletsOf ts where
	inletsOf :: T.Num idx => ts -> idx -> C.ObjectRef -> T.MapR Inlet ts
instance InletsOf T.Nil where
	inletsOf _ _ _ = T.Nil
instance (Type h, InletsOf t) => InletsOf (T.Cons h t) where
	inletsOf (T.Cons h t) n obj = T.Cons (Inlet h (obj C.@- T.intOfNum n)) (inletsOf t (T.S n) obj)

class (ListOfType ts) => OutletsOf ts where
	outletsOf :: T.Num idx => ts -> idx -> C.ObjectRef -> T.MapR Outlet ts
instance OutletsOf T.Nil where
	outletsOf _ _ _ = T.Nil
instance (Type h, OutletsOf t) => OutletsOf (T.Cons h t) where
	outletsOf (T.Cons h t) n obj = T.Cons (Outlet h (obj C.@+ T.intOfNum n)) (outletsOf t (T.S n) obj)

wrap :: (InletsOf li, OutletsOf lo) => C.Object -> li -> lo -> Pd (Object li lo)
wrap obj ins outs = do
	ref <- C.insertObject obj
	return$ Object (inletsOf ins T.Z ref) (outletsOf outs T.Z ref)
