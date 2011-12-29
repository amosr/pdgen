import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Lib as L
import qualified Language.Pd.PdGen.Lib.Trigger as LT
import qualified Language.Pd.PdGen.Types as T
import Language.Pd.PdGen.Out

sustain :: C.PdState
sustain = C.patch$ do
	note <- L.inlet PdNum "note"
	vel <- L.inlet PdNum "vel"
	pedal <- L.inlet PdNum "pedal"

	swap <- L.swap

	note@+T.n0 @-> swap@-T.n0
	vel@+T.n0 @-> swap@-T.n1

	pack <- L.pack (T.l2 0 0)
	swap@+T.n0 @-> pack@-T.n0
	swap@+T.n1 @-> pack@-T.n1

	route <- L.route1 "0"
	pack@+T.n0 @-> route@-T.n0

	dam <- wrap (C.Object "dam" []) (T.l2 PdAny PdAny) (T.l2 PdAny PdAny)
	route@+T.n0 @-> dam@-T.n0

	eqz <- L.eq 0
	pedal@+T.n0 @-> eqz@-T.n0
	eqz@+T.n0 @-> dam@-T.n1

	tfb <- LT.trigger (T.l2 LT.TFloat LT.TBang)
	dam@+T.n0 @-> tfb@-T.n0


	outnote <- L.outlet PdNum "note"
	outvel <- L.outlet PdNum "vel"

	tfb@+T.n0 @-> outnote@-T.n0

	mz <- L.message "0"
	tfb@+T.n1 @-> mz@-T.n0
	mz@+T.n0 `forceInto` outvel@-T.n0

	unpack <- LT.unpack (T.l2 LT.TFloat LT.TFloat)
	route@+T.n1 @-> unpack@-T.n0

	unpack@+T.n0 @-> outnote@-T.n0
	unpack@+T.n1 @-> outvel@-T.n0

	return ()

main = do
	print (generate "sustain" sustain)

