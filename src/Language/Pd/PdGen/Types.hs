{-# OPTIONS -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XTypeFamilies -XRankNTypes -XUndecidableInstances #-}
module Language.Pd.PdGen.Types where

data Z = Z deriving (Eq,Show)
data S n = S n deriving (Eq,Show)

class Num n
instance Num Z
instance Num n => Num (S n)

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


class (List l) => Map m l where
	type MapR m l
	tmap :: (forall a. a -> m a) -> l -> MapR m l
instance  Map m Nil where
	type MapR m Nil = Nil
	tmap _ _ = Nil
instance (Map m t) => Map m (Cons h t)  where
	type MapR m (Cons h t) = Cons (m h) (MapR m t)
	tmap f (Cons h t) = Cons (f h) (tmap f t)

data Eq
data NotEq

type family TypeEq a b
type instance TypeEq a a = Eq
type instance TypeEq a b = NotEq
