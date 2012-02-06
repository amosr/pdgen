{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}
module Language.Pd.PdGen.Out (out, write, generate) where

import Data.Maybe
import System.Directory

import Language.Pd.PdGen.Core
import Language.Pd.PdGen.Out.Layout

out :: String -> PdState -> IO ()
out name pd = write (generate name pd)

-- | Get list of filenames and file contents for patch.
-- Multiple files will be returned if patch has subpatches.
generate :: String -- ^ name
	-> PdState -- ^ state containing patch and subpatches
	-> [(String,String)] -- ^ file name and contents pairs
generate name (PdState{psPatch = patch, psSubs = subs}) =
	first : map subpatch subs
	where
	subfiles = map (\s -> (spId s, subname s)) subs
	subname s = name ++ "/" ++ spName s ++ "-" ++ show (spId s)

	subfiles_nodir = map (\s -> (spId s, subname_nodir s)) subs
	subname_nodir s = spName s ++ "-" ++ show (spId s)

	first = (name, outPatch name patch subfiles)
	
	subpatch s = let sn = subname s in
		(sn, outPatch sn (spPatch s) subfiles_nodir)

write :: [(String,String)] -> IO ()
write = mapM_ write1
write1 (name,contents) = do
	let dir = reverse (dropWhile (/='/')$ reverse name)
	createDirectoryIfMissing True dir
	writeFile (name++".pd") contents

outPatch :: String -- ^ patch's filename, excluding .pd
	-> Patch -- ^ patch
	-> [(Int,String)] -- ^ map of subpatch id to path (eg synth/vox for synth/vox.pd)
	-> String -- ^ contents of file
outPatch name p subs = unlines (prelude name ++ outObjects p subs ++ outConnections p)

prelude :: String -> [String]
prelude name = ["#N canvas 0 0 100 100 pdgen-" ++ name ++ ";"]

outObjects :: Patch -> [(Int,String)] -> [String]
outObjects p subs = map (outObject subs) (objects p `zip` (map snd positions))
	where positions = layout p

outObject :: [(Int,String)] -> (Object,(Int,Int)) -> String
outObject _ (Object name args, v) = obj "obj" v (unwords (name:args))
outObject _ (Message msg, v) = obj "msg" v msg
outObject _ (FloatAtom, v) = obj "floatatom" v "5 0 0 0 - - -"
outObject _ (SymbolAtom, v) = obj "symbolatom" v "5 0 0 0 - - -;"
outObject subs (ObjSubPatch id args, v) = obj "obj" v (unwords (name:args))
	where name = fromMaybe (error$ "subpatch id " ++ show id) (lookup id subs)

obj :: String -> (Int,Int) -> String -> String
obj ty (x,y) rest = unwords ["#X", ty, show x, show y, rest, ";"]

outConnections :: Patch -> [String]
outConnections p = map outConnection (connections p)
outConnection :: (Outlet,Inlet) -> String
outConnection (Outlet (ObjectRef from, fromO), Inlet (ObjectRef to, toI)) = "#X connect " ++ unwords (map show [from, fromO, to, toI]) ++ ";"
