import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Connectors as Cn
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Extra
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Out as O

mkSweeper :: Int -> Int -> Int
	  -> Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	  -> Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
mkSweeper 0 _ _ syn = syn
mkSweeper n freqd delay syn = do
    freq <- number
    vel <- number

    s <- syn
    Cn.binop (freq,vel) s

    sumL <- addS
    s@+T.n0 @-> sumL@-T.n0
    sumR <- addS
    s@+T.n1 @-> sumR@-T.n0

    p <- pipe (T.l2 TFloat TFloat) delay
    freq@+T.n0 @-> p@-T.n0
    vel@+T.n0 @-> p@-T.n1

    freq' <- add1 freqd
    p@+T.n0 @-> freq'@-T.n0

    subsweep <- mkSweeper (n-1) freqd delay syn
    freq'@+T.n0 @-> subsweep@-T.n0
    p@+T.n1 @-> subsweep@-T.n1

    subsweep@+T.n0 @-> sumL@-T.n0
    subsweep@+T.n1 @-> sumR@-T.n0

    return $ Object (T.l2 (freq@-T.n0) (vel@-T.n0)) (T.l2 (sumL@+T.n0) (sumR@+T.n0))

syn = subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "syn"$ do
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

sweeper phases freqd delay = subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "sweeper"$ do
	freq <- inlet PdNum "freq" `Cn.com1` mtof `Cn.com1` number 
	vel <- inlet PdNum "vel" `Cn.com1` div1 127 `Cn.com1` number

	vox <- syn
	let s2 = mkSweeper 8 (-1) 15 (vox [])
	swp <- mkSweeper phases freqd delay s2

	Cn.binop (freq,vel) swp

	outL <- outletS "left"
	swp@+T.n0 @-> outL@-T.n0
	outR <- outletS "right"
	swp@+T.n1 @-> outR@-T.n0

	return ()
	

sall = patch$ do
	sbassR <- sweeper 8 2 150 --500

	notes <- notein

	notesub <- sub1 12
	notes@+T.n0 @-> notesub@-T.n0

	sbass <- polysynth T.n2 (sbassR []) :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	out <- dacS2

	Cn.manyToMany (T.l2 (notesub@+T.n0) (notes@+T.n1)) (inlets sbass)
	Cn.manyToMany (outlets sbass) (inlets out)

main = O.out "synth-sweep" sall



