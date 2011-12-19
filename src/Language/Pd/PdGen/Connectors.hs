module Language.Pd.PdGen.Connectors where

import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Types as T


class (Type t, T.List l) => OneToMany t l where
	oneToMany :: Outlet t -> l -> Pd ()
instance (Type t) => OneToMany t T.Nil where
	oneToMany p i = return ()
instance (Type t, ConnectInto t (TypeOfInlet h), OneToMany t r, h ~ Inlet hty) => OneToMany t (T.Cons h r) where
	oneToMany p (T.Cons h r) =
		p @-> h >> oneToMany p r

{-
class (T.List is, T.List os) => ManyToMany is os where
	manyToMany :: is -> os -> Pd ()
instance (Type t) => OneToMany t T.Nil where
	oneToMany p i = return ()
instance (Type t, ConnectInto t (TypeOfInlet h), OneToMany t r, h ~ Inlet hty) => ConnectAll t (T.Cons h r) where
	oneToMany p (T.Cons h r) =
		p @-> h >> oneToMany p r


-}
