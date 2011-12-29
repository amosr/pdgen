import Language.Pd.PdGen.Core
import Language.Pd.PdGen.Out

sustain :: PdState
sustain = patch$ do
	note <- object "inlet" ["note"]
	vel <- object "inlet" ["vel"]
	pedal <- object "inlet" ["pedal"]

	swap <- object "swap" []

	note@+0 `into` swap@-0
	vel@+0 `into` swap@-1

	pack <- object "pack" ["0", "0"]
	swap@+0 `into` pack@-0
	swap@+1 `into` pack@-1

	route <- object "route" ["0"]
	pack@+0 `into` route@-0

	dam <- object "dam" []
	route@+0 `into` dam@-0

	eqz <- object "==" ["0"]
	pedal@+0 `into` eqz@-0
	eqz@+0 `into` dam@-1

	tfb <- object "t" ["f", "b"]
	dam@+0 `into` tfb@-0


	outnote <- object "outlet" ["note"]
	outvel <- object "outlet" ["vel"]

	tfb@+0 `into` outnote@-0

	mz <- message "0"
	tfb@+1 `into` mz@-0
	mz@+0 `into` outvel@-0

	unpack <- object "unpack" []
	route@+1 `into` unpack@-0

	unpack@+0 `into` outnote@-0
	unpack@+1 `into` outvel@-0

	return ()

main = do
	print (generate "sustain" sustain)
