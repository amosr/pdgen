import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Connectors as Cn
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Out as O

moses split = wrapObj "moses" [num split] (T.l2 PdNum PdNum) (T.l2 PdNum PdNum)

pipeF del = wrapObj "pipe" [num del] (T.l2 PdAny PdNum) (T.l1 PdNum)
delwriteS nm maxtime = wrapObj "delwrite~" [nm, num maxtime] (T.l1 PdSig) (T.Nil)
vdS nm = wrapObj "vd~" [nm] (T.l1 PdSig) (T.l1 PdSig)
phasorS = wrapObj "phasor~" [] (T.l2 PdSig PdNum) (T.l1 PdSig)
cosS = wrapObj "cos~" [] (T.l1 PdSig) (T.l1 PdSig)
sinS = wrapObj "sin~" [] (T.l1 PdSig) (T.l1 PdSig)

gt1 f = wrapObj ">" [num f] (T.l2 PdNum PdNum) (T.l1 PdNum)

svfS ty = wrapObj "svf~" [ty] (T.l4 PdSig PdSig PdSig PdSig) (T.l1 PdSig)

adsr :: Float -> Float -> Float -> Float -> Pd (Object (T.L1 PdNum) (T.L1 PdSig))
adsr a d s r = do
	t <- moses 0.001

	let off = t@+T.n0
	let on = t@+T.n1
	
	l <- lineS

	msgA <- message$ "\\$1 " ++ num a
	on @-> msgA@-T.n0
	msgA@+T.n0 @-> l@-T.n0
	msgD <- pipeF a `Cn.com1` mul1 s `Cn.com1` message ("\\$1 " ++ num d)
	on @-> msgD@-T.n0
	msgD@+T.n0 @-> l@-T.n0

	msgR <- message$ "0 " ++ num r
	off @-> msgR@-T.n0
	msgR@+T.n0 @-> l@-T.n0
	
	clear <- message "clear"
	off @-> clear@-T.n0
	clear@+T.n0 @-> msgD@-T.n0

	return$ Object (T.l1 (t@-T.n0)) (T.l1 (l@+T.n0))

-- frequency to milliseconds
ftoms :: Pd (Object (T.L1 PdAny) (T.L1 PdNum))
ftoms = do
	t <- trigger (T.l2 TBang TFloat)
	m <- message "1000"
	t@+T.n0 @-> m@-T.n0

	d <- div1 1
	-- I promise that m is a number
	m@+T.n0 `forceInto` d@-T.n0
	t@+T.n1 @-> d@-T.n1

	return$ Object (T.l1 (t@-T.n0)) (T.l1 (d@+T.n0))

bass = subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "bass"$ do
	freq <- inlet PdNum "freq" `Cn.com1` mtof `Cn.com1` number 
	vel <- inlet PdNum "vel" `Cn.com1` div1 127 `Cn.com1` number

	oscvel <- return vel `Cn.com1` adsr 10.0 100.0 0.5 100.0
	filtervel <- return vel `Cn.com1` gt1 0 `Cn.com1` mul1 2 `Cn.com1` adsr 0 250 0.3 500

	osc <- return freq `Cn.com1` oscS
	oscenv <- mulS
	Cn.binop (osc,vel) oscenv

	delhz <- return freq `Cn.com1` message "\\$1 10" `Cn.com1` lineS
	delms <- return freq `Cn.com1` ftoms `Cn.com1` number `Cn.com1` message "\\$1 10" `Cn.com1` lineS
	delosc <- return delhz `Cn.com1` divS1 5 `Cn.com1` oscS `Cn.com1` divS1 2
	delmul <- mulS
	Cn.binop (delms,delosc) delmul
	deladd <- addS
	Cn.binop (delms,delmul) deladd
	delread <- return deladd `Cn.com1` vdS "\\$0-del"

	filtmul <- mulS
	Cn.binop (filtervel,delhz) filtmul
	filt <- svfS "low"
	oscenv@+T.n0 @-> filt@-T.n0
	filtmul@+T.n0 @-> filt@-T.n1

	o <- oscS1 17 `Cn.com1` addS1 6 `Cn.com1` divS1 7
	o@+T.n0 @-> filt@-T.n2
	o@+T.n0 @-> filt@-T.n3



	sum <- addS
	Cn.binop (filt,delread) sum 

	delwrite <- return sum `Cn.com1` mulS1 0.8 `Cn.com1` delwriteS "\\$0-del" 1000

	out <- return sum `Cn.com1` sinS
	outL <- return out `Cn.com1` outletS "left"
	outR <- return out `Cn.com1` outletS "right"

	return ()
	

sall = patch$ do
	sbassR <- bass

	notes <- notein
	sbass <- polysynth T.n1 (sbassR []) :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	out <- dacS2

	Cn.manyToMany (T.l2 (notes@+T.n0) (notes@+T.n1)) (inlets sbass)
	Cn.manyToMany (outlets sbass) (inlets out)

main = O.out "all" sall


