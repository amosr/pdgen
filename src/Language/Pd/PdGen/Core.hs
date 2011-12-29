module Language.Pd.PdGen.Core where

import Control.Monad.State

data Object
	= Object String [String]
	| Message String
	| FloatAtom -- todo lower, upper bounds, send/rcv names
	| SymbolAtom
	| ObjSubPatch Int [String]
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

data SubPatch = SubPatch {
	spPatch :: Patch,
	spName :: String,
	spId :: Int }
	deriving (Eq,Show)

data PdState = PdState {
	psPatch :: Patch,
	psSubs :: [SubPatch],
	psSubNextId :: Int }
	deriving (Eq,Show)

empty :: Int -> PdState
empty id = PdState (Patch [] []) [] id

objectRefs :: Patch -> [ObjectRef]
objectRefs p = map ObjectRef [0..length (objects p)]

type Pd a = State PdState a

patch :: Pd a -> PdState
patch p = snd$ runState p (empty 1)

subpatch :: String -> Pd a -> Pd ([String] -> Pd ObjectRef)
subpatch name sub = do
	-- get current subpatches & counter
	state@(PdState{psSubs = subs, psSubNextId = id}) <- get
	-- run the subpatch in new state with incremented id counter
	let subSt = snd$ runState sub (empty (id+1))
	let subPa = SubPatch (psPatch subSt) name id
	let subSubs = subPa : psSubs subSt
	-- insert new subpatches and update id counter
	put (state{psSubs = subs ++ subSubs, psSubNextId = (psSubNextId subSt)})
	-- return an inserter
	return$ insertObject . ObjSubPatch id

insertObject :: Object -> Pd ObjectRef
insertObject o = do
	pdst@(PdState { psPatch = p@Patch { objects = objs }}) <- get
	put (pdst{psPatch = p{ objects = objs ++ [o] }})
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
	pdst@(PdState { psPatch = p@Patch { connections = conns }}) <- get
	put (pdst{psPatch = p{ connections = conns ++ [(o,i)] }})
