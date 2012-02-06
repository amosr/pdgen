import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Connectors as Cn
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Extra
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Out as O

-- | turn vox into a simple arpeggio kind of vox
mkSweeper :: Int -- ^ number of iterations
	  -> Int -- ^ semitone increase
	  -> Int -- ^ delay
	  -> Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig)) -- ^ voice
	  -> Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
mkSweeper 0 _ _ syn = syn
mkSweeper n freqd delay syn = do
    -- faux inlets
    freq <- number
    vel <- number

    -- instantiate vox and hook into inlets
    s <- syn
    Cn.binop (freq,vel) s

    -- get output of vox
    sumL <- addS
    s@+T.n0 @-> sumL@-T.n0
    sumR <- addS
    s@+T.n1 @-> sumR@-T.n0

    -- delay freq & vel messages
    p <- pipe (T.l2 TFloat TFloat) delay
    freq@+T.n0 @-> p@-T.n0
    vel@+T.n0 @-> p@-T.n1
    -- increase delayed freq
    freq' <- add1 freqd
    p@+T.n0 @-> freq'@-T.n0
    -- iterate
    subsweep <- mkSweeper (n-1) freqd delay syn
    freq'@+T.n0 @-> subsweep@-T.n0
    p@+T.n1 @-> subsweep@-T.n1

    -- connect child into output
    subsweep@+T.n0 @-> sumL@-T.n0
    subsweep@+T.n1 @-> sumR@-T.n0

    return $ Object (T.l2 (freq@-T.n0) (vel@-T.n0)) (T.l2 (sumL@+T.n0) (sumR@+T.n0))

-- | simple vca'd sine
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

-- | create a 'magical' sound
sweeper = subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "sweeper"$ do
    freq <- inlet PdNum "freq" `Cn.com1` mtof `Cn.com1` number 
    vel <- inlet PdNum "vel" `Cn.com1` div1 127 `Cn.com1` number

    -- instantiate syn since it's a subpatch
    vox <- syn
    -- create arpeggios
    let s2 = mkSweeper 16 (-2) 95 (vox [])
    swp <- mkSweeper 16 3 50 s2

    Cn.binop (freq,vel) swp

    -- create delay line ala crappy string sim
    del <- delwriteS "\\$0-dL" 1000
    swp@+T.n0 @-> del@-T.n0

    -- read from delay by frequency and then feed back
    -- .. this should have a mtof (midi to freq) then freq to milliseconds?
    vd <- return freq `Cn.com1` ftoms `Cn.com1` vdS "\\$0-dL"
    feedback <- return vd `Cn.com1` mulS1 0.90 `Cn.com1` return del

    -- sum arpeggio and delay line for output
    outL <- outletS "left"
    swp@+T.n0 @-> outL@-T.n0
    vd@+T.n0 @-> outL@-T.n0
    outR <- outletS "right"
    swp@+T.n1 @-> outR@-T.n0
    vd@+T.n0 @-> outR@-T.n0

    return ()
	

sall = patch$ do
    sbassR <- sweeper

    -- get from midi
    notes <- notein

    -- transpose down an octave
    notesub <- sub1 12
    notes@+T.n0 @-> notesub@-T.n0

    -- make it polyphonic
    sbass <- polysynth T.n2 (sbassR []) :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
    out <- dacS2

    -- connect everything
    Cn.manyToMany (T.l2 (notesub@+T.n0) (notes@+T.n1)) (inlets sbass)
    Cn.manyToMany (outlets sbass) (inlets out)

main = O.out "synth-sweep" sall



