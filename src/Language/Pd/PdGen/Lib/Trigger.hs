module Language.Pd.PdGen.Lib.Trigger where

import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Types as T

data TFloat = TFloat
data TBang = TBang
data TSymbol = TSymbol
data TList = TList
data TAnything = TAnything

class (Type (TriggerTypeR t)) => TriggerType t where
	type TriggerTypeR t
	triggerType :: t -> TriggerTypeR t
	triggerTypeStr :: t -> String

instance TriggerType TFloat where
	type TriggerTypeR TFloat = PdNum
	triggerType _ = PdNum
	triggerTypeStr _ = "float"
instance TriggerType TBang where
	type TriggerTypeR TBang = PdBang
	triggerType _ = PdBang
	triggerTypeStr _ = "bang"
instance TriggerType TSymbol where
	type TriggerTypeR TSymbol = PdSymbol
	triggerType _ = PdSymbol
	triggerTypeStr _ = "symbol"
instance TriggerType TList where
	type TriggerTypeR TList = PdAny
	triggerType _ = PdAny
	triggerTypeStr _ = "list"
instance TriggerType TAnything where
	type TriggerTypeR TAnything = PdAny
	triggerType _ = PdAny
	triggerTypeStr _ = "anything"

class (T.List l, ListOfType (MapTriggersR l)) => MapTriggers l where
	type MapTriggersR l
	mapTriggers :: l -> (MapTriggersR l,[String])
instance MapTriggers T.Nil where
	type MapTriggersR T.Nil = T.Nil
	mapTriggers _ = (T.Nil,[])
instance (TriggerType h, MapTriggers t) => MapTriggers (T.Cons h t) where
	type MapTriggersR (T.Cons h t) = T.Cons (TriggerTypeR h) (MapTriggersR t)
	mapTriggers (T.Cons h t) = let (tys,strs) = mapTriggers t in
		(T.Cons (triggerType h) tys, triggerTypeStr h : strs)


trigger toobs = let (tys,strs) = mapTriggers toobs in
	wrap (C.Object "trigger" strs)
		(T.l1 PdAny) tys

-- todo this should be only float,symbol,pointer?
unpack toobs =  let (tys,strs) = mapTriggers toobs in
	wrap (C.Object "unpack" strs)
		(T.l1 PdAny) tys

unpack2F = unpack (T.l2 TFloat TFloat)

packT toobs =  let (tys,strs) = mapTriggers toobs in
	wrap (C.Object "pack" strs)
		tys (T.l1 PdAny)

