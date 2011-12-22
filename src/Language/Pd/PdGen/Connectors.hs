module Language.Pd.PdGen.Connectors where

import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Types as T


class (Type t, T.List l) => OneToMany t l where
	oneToMany :: Outlet t -> l -> Pd ()
instance (Type t) => OneToMany t T.Nil where
	oneToMany p i = return ()
instance (Type t, ConnectInto t hty, OneToMany t r, h ~ Inlet hty) => OneToMany t (T.Cons h r) where
	oneToMany p (T.Cons h r) =
		p @-> h >> oneToMany p r

class (T.List is, T.List os) => ManyToMany os is where
	manyToMany :: os -> is -> Pd ()
instance ManyToMany T.Nil T.Nil where
	manyToMany _ _ = return ()
instance (ConnectInto oty ity, ManyToMany os is, o ~ Outlet oty, i ~ Inlet ity) =>
		ManyToMany (T.Cons o os) (T.Cons i is) where
	manyToMany (T.Cons o os) (T.Cons i is) =
		o @-> i >> manyToMany os is

com1 :: (ConnectInto o i,
	T.List (T.MapR Inlet in1), T.List (T.MapR Outlet out1),
	T.List (T.MapR Inlet in2), T.List (T.MapR Outlet out2),
	ListOfType in1, ListOfType out1,
	ListOfType in2, ListOfType out2) =>
	Pd (Object in1 (T.Cons o out1)) -> 
	Pd (Object (T.Cons i in2) out2) ->
	Pd (Object in1 out2)
com1 f g = do
	f' <- f
	g' <- g
	f'@+T.n0 @-> g'@-T.n0
	return$ Object (inlets f') (outlets g')

binop (p,q) op = do
	p@+T.n0 @-> op@-T.n0
	q@+T.n0 @-> op@-T.n1
