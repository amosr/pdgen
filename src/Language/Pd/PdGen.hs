{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen where

import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Types as T

-- | Float
data PdNum = PdNum deriving (Eq,Show)
data PdBang = PdBang deriving (Eq,Show)
data PdSymbol = PdSymbol deriving (Eq,Show)
-- | Any control data. Lists.
data PdAny = PdAny deriving (Eq,Show)
-- | Float signals, audio rate
data PdSig = PdSig deriving (Eq,Show)

-- | Pd inlet/outlet type
class Type p
instance Type PdNum
instance Type PdBang
instance Type PdSymbol
instance Type PdAny
instance Type PdSig

-- | Which outlet types are accepted by inlets.
-- Can connect a 'PdNum' into a 'PdAny', but not the other way around.
class (Type o, Type i) => ConnectInto o i
instance (Type t) => ConnectInto t t

instance ConnectInto PdNum PdAny
instance ConnectInto PdBang PdAny
instance ConnectInto PdSymbol PdAny

instance ConnectInto PdNum PdSig

-- | An inlet of type p, can only connect outlets with @'ConnectInto' n p@
data Inlet p = Inlet p C.Inlet deriving (Eq,Show)
-- | An outlet of type p, can only connect into inlets with @'ConnectInto' p n@
data Outlet p = Outlet p C.Outlet deriving (Eq,Show)

-- | A type-list whose elements are all pd types; PdNum, PdAny etc.
class ListOfType l
instance ListOfType T.Nil
instance (Type h, ListOfType t) => ListOfType (T.Cons h t)

-- | An object with specific types of inlets and outlets.
-- The map turns out to be kind of annoying for type inference, sometimes:
-- it can't go from 
--
-- > Cons (Inlet PdNum) (Cons (Inlet PdAny) Nil)
--
-- to
--
-- > Cons PdNum (Cons PdAny Nil)
--
-- because it isn't sure @MapR Inlet@ is injective.
data Object li lo = Object {
	inlets :: T.MapR Inlet li,
	outlets :: T.MapR Outlet lo }

-- | Get nth inlet. Use 'T.n0' etc.
infixl 5 @-
(@-) :: T.Take (T.MapR Inlet li) n =>Object li lo -> n -> T.TakeR (T.MapR Inlet li) n
p @- n = T.take (inlets p) n
-- | Get nth outlet. Use 'T.n0' etc.
infixl 5 @+
(@+) :: T.Take (T.MapR Outlet lo) n =>Object li lo -> n -> T.TakeR (T.MapR Outlet lo) n
p @+ n = T.take (outlets p) n

type Pd a = C.Pd a

-- | Connect an outlet into an inlet.
--
-- > osc <- oscS
-- > dac <- dacS2
-- > osc@+T.n0 @-> dac@-T.n0
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

-- | Wrap an untyped pd object with typed inlets and outlets.
--
-- > wrap (C.object "osc~" []) (T.l1 PdSig) (T.l1 PdSig)
wrap :: (InletsOf li, OutletsOf lo) => C.Object -> li -> lo -> Pd (Object li lo)
wrap obj ins outs = do
	ref <- C.insertObject obj
	return$ Object (inletsOf ins T.Z ref) (outletsOf outs T.Z ref)

patch = C.patch

-- | create subpatch
-- TODO: smarter one that takes a @Pd (Object lis los)@ and creates inlets for each li and outlets for los..
subpatch :: (InletsOf li, OutletsOf lo) => li -> lo -> String -> Pd () -> Pd ([String] -> Pd (Object li lo))
subpatch ins outs name sub = do
	inserter <- C.subpatch name sub
	return (\args -> do
		ref <- inserter args
		return$ Object (inletsOf ins T.Z ref) (outletsOf outs T.Z ref))

