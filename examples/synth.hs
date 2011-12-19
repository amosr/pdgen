import Language.Pd.PdGen
import qualified Language.Pd.PdGen.Connectors as Cn
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T

import qualified Language.Pd.PdGen.Core as C
import qualified Language.Pd.PdGen.Out as O

rhodes = C.patch$ do
	notes <- notein
	p <- polysynth T.n8 vox :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
	out <- dacS2

	Cn.manyToMany (T.l2 (notes@+T.n0) (notes@+T.n1)) (inlets p)
	Cn.manyToMany (outlets p) (inlets out)

	


vox :: Pd (Object (T.L2 PdNum PdNum) (T.L2 PdSig PdSig))
vox = do
	freq <- mtof

	vel <- div1 1270
	msg <- message "\\$1 10"
	vel@+T.n0 @-> msg@-T.n0
	line <- lineS
	msg@+T.n0 @-> line@-T.n0

	osc1 <- oscS
	freq@+T.n0 @-> osc1@-T.n0

	osc2ctl_mul <- mulS1 50
	osc1@+T.n0 @-> osc2ctl_mul@-T.n0

	osc2ctl_add <- addS
	osc2ctl_mul@+T.n0 @-> osc2ctl_add@-T.n0
	freq@+T.n0 @-> osc2ctl_add@-T.n1

	osc2 <- oscS
	osc2ctl_add@+T.n0 @-> osc2@-T.n0

	mul <- mulS
	Cn.manyToMany (T.l2 (osc2@+T.n0) (line@+T.n0)) (inlets mul)
	osc1@+T.n0 @-> mul@-T.n0

	return$ Object (T.l2 (freq@-T.n0) (vel@-T.n0)) (T.l2 (mul@+T.n0) (mul@+T.n0))

main = putStrLn (O.out rhodes)
