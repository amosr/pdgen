module Language.Pd.PdGen.Core where

import Control.Monad.State

data Object
	= Object String [String]
	| Message String
	| FloatAtom -- todo lower, upper bounds, send/rcv names
	| SymbolAtom
	-- text? array?
	deriving (Eq,Show)
newtype ObjectRef = ObjectRef Int
	deriving (Eq,Show)
	
type Port = (ObjectRef,Int)
newtype Inlet = Inlet Port
	deriving (Eq,Show)
newtype Outlet = Outlet Port
	deriving (Eq,Show)

data Patch = Patch {
	objects :: [Object],
	connections :: [(Outlet,Inlet)] }
	deriving (Eq,Show)

empty :: Patch
empty = Patch [] []

objectRefs :: Patch -> [ObjectRef]
objectRefs p = map ObjectRef [0..length (objects p)]

type Pd a = State Patch a

patch :: Pd a -> Patch
patch p = snd$ runState p empty

insertObject :: Object -> Pd ObjectRef
insertObject o = do
	p@(Patch { objects = objs }) <- get
	put (p{ objects = objs ++ [o] })
	return$ ObjectRef (length objs)

object :: String -> [String] -> Pd ObjectRef
object name args = insertObject$ Object name args

message :: String -> Pd ObjectRef
message = insertObject . Message
-- etc...

infixl 9 @+
(@+) :: ObjectRef -> Int -> Outlet
(@+) o num = Outlet (o,num)

infixl 9 @-
(@-) :: ObjectRef -> Int -> Inlet
(@-) o num = Inlet (o,num)

infixl 5 `into`
into :: Outlet -> Inlet -> Pd ()
into o i = do
	p@(Patch { connections = conns }) <- get
	put (p{ connections = conns ++ [(o,i)] })
