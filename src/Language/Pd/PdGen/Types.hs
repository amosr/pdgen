{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen.Types where

import Prelude hiding (take,Num,map)

data Z = Z deriving (Eq)
data S n = S n deriving (Eq)

class Num n where
	intOfNum :: n -> Int
instance Num Z where
	intOfNum _ = 0
instance Num n => Num (S n) where
	intOfNum (S n) = (intOfNum n) + 1

instance Num n => Show n where
	show = show . intOfNum

type N0 = Z
type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

n0 = Z
n1 = S Z
n2 = S n1
n3 = S n2
n4 = S n3
n5 = S n4
n6 = S n5
n7 = S n6
n8 = S n7

type family Add a b
type instance Add Z b = b
type instance Add (S a) b = S (Add a b)


data Nil = Nil deriving (Eq,Show)
data Cons h t = Cons h t deriving (Eq,Show)

type L1 a = Cons a Nil
l1 a = Cons a Nil
type L2 a b = Cons a (L1 b)
l2 a b = Cons a (l1 b)
type L3 a b c = Cons a (L2 b c)
l3 a b c = Cons a (l2 b c)
type L4 a b c d = Cons a (L3 b c d)
l4 a b c d = Cons a (l3 b c d)

class List n
instance List Nil
instance List (Cons h t)

class (List l, Num n) => Take l n where
	type TakeR l n 
	take :: l -> n -> TakeR l n
instance (List t) => Take (Cons h t) Z where
	type TakeR (Cons h t) Z = h
	take (Cons h t) Z = h
instance (Num n, List t, Take t n) => Take (Cons h t) (S n) where
	type TakeR (Cons h t) (S n) = TakeR t n
	take (Cons h t) (S n) = take t n

class (List a, List b, List (AppR a b)) => App a b where
	type AppR a b
	app :: a -> b -> AppR a b
instance (List b) => App Nil b where
	type AppR Nil b = b
	app _ b = b
instance (App a b) => App (Cons h a) b where
	type AppR (Cons h a) b = Cons h (AppR a b)
	app (Cons h a) b = Cons h (app a b)

class (Num t, List (Range0R t)) => Range0 t where
	type Range0R t
	range0 :: t -> Range0R t
instance Range0 Z where
	type Range0R Z = Nil
	range0 _ = Nil
instance (Range0 t, App (Range0R t) (L1 (S t))) => Range0 (S t) where
	type Range0R (S t) = AppR (Range0R t) (L1 (S t))
	range0 (S t) = app (range0 t) (l1$ S t)

class (List l) => Map m l where
	type MapR m l
	map :: (forall a. a -> m a) -> l -> MapR m l
instance  Map m Nil where
	type MapR m Nil = Nil
	map _ _ = Nil
instance (Map m t) => Map m (Cons h t)  where
	type MapR m (Cons h t) = Cons (m h) (MapR m t)
	map f (Cons h t) = Cons (f h) (map f t)

{-
data TEq = TEq
data TNotEq = TNotEq

type family TypeEq a b
type instance TypeEq a a = TEq
type instance TypeEq a b = TNotEq
-}
