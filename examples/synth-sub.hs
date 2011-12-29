import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Connectors as Cn
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Out as O

synth = patch$ do
	svox <- subpatch (T.l2 PdNum PdNum) (T.l2 PdSig PdSig) "vox" vox
	notes <- notein
	p <- polysynth T.n8 (svox []) :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	out <- dacS2

	Cn.manyToMany (T.l2 (notes@+T.n0) (notes@+T.n1)) (inlets p)
	Cn.manyToMany (outlets p) (inlets out)


vox :: Pd ()
vox = do
	freq <- inlet PdNum "freq" `Cn.com1` mtof
	vel <- inlet PdNum "v" `Cn.com1` number

	env <- div1 1270 `Cn.com1` message "\\$1 10" `Cn.com1` lineS
	vel@+T.n0 @-> env@-T.n0

	osc1 <- oscS
	freq@+T.n0 @-> osc1@-T.n0

	osc2ctl_mul <- mulS
	env2 <- mul1 500 `Cn.com1` message "0 \\, \\$1 5000" `Cn.com1` lineS
	env2@+T.n0 @-> osc2ctl_mul@-T.n1
	vel@+T.n0 @-> env2@-T.n0

	osc1@+T.n0 @-> osc2ctl_mul@-T.n0

	osc2ctl_add <- addS
	Cn.binop (osc2ctl_mul,freq) osc2ctl_add 

	osc2 <- oscS
	osc2ctl_add@+T.n0 @-> osc2@-T.n0

	mul <- mulS
	Cn.binop (osc2, env) mul
	osc1@+T.n0 @-> mul@-T.n0

	outL <- outletS "left"
	outR <- outletS "right"

	Cn.manyToMany (T.l2 (mul@+T.n0) (mul@+T.n0)) (T.l2 (outL@-T.n0) (outR@-T.n0))

	return ()

main = O.out "synth-sub" synth

