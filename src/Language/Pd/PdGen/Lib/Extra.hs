module Language.Pd.PdGen.Lib.Extra where

import Language.Pd.PdGen
import Language.Pd.PdGen.Lib
import Language.Pd.PdGen.Lib.Trigger
import qualified Language.Pd.PdGen.Types as T
import qualified Language.Pd.PdGen.Connectors as Cn

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

