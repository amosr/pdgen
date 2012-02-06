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
	vel <- inlet PdNum "vel" `Cn.com1` div1 256 `Cn.com1` number

	oscvel <- return vel `Cn.com1` adsr 10.0 100.0 0.5 100.0

	osc <- return freq `Cn.com1` oscS
	oscenv <- mulS
	Cn.binop (osc,vel) oscenv

	out <- return oscenv `Cn.com1` sinS
	outL <- return out `Cn.com1` outletS "left"
	outR <- return out `Cn.com1` outletS "right"

	return ()

sweeper = subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "sweeper"$ do
	freq <- inlet PdNum "freq" `Cn.com1` mtof `Cn.com1` number 
	vel <- inlet PdNum "vel" `Cn.com1` div1 127 `Cn.com1` number

	vox <- syn
	let s2 = mkSweeper 16 (-2) 95 (vox [])
	swp <- mkSweeper 16 3 50 s2

	Cn.binop (freq,vel) swp

	del <- delwriteS "\\$0-dL" 1000
	swp@+T.n0 @-> del@-T.n0

	vd <- return freq `Cn.com1` ftoms `Cn.com1` vdS "\\$0-dL"
	feedback <- return vd `Cn.com1` mulS1 0.90 `Cn.com1` return del

	outL <- outletS "left"
	swp@+T.n0 @-> outL@-T.n0
	vd@+T.n0 @-> outL@-T.n0
	outR <- outletS "right"
	swp@+T.n1 @-> outR@-T.n0
	vd@+T.n0 @-> outR@-T.n0

	return ()
	

sall = patch$ do
	sbassR <- sweeper

	notes <- notein

	notesub <- sub1 12
	notes@+T.n0 @-> notesub@-T.n0

	sbass <- polysynth T.n2 (sbassR []) :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	out <- dacS2

	Cn.manyToMany (T.l2 (notesub@+T.n0) (notes@+T.n1)) (inlets sbass)
	Cn.manyToMany (outlets sbass) (inlets out)

main = O.out "synth-sweep" sall



