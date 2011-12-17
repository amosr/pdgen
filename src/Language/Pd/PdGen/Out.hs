{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}
module Language.Pd.PdGen.Out where

import Language.Pd.PdGen.Core

import qualified GraphRewriting.Graph as G
import qualified GraphRewriting.Graph.Write as G
import qualified GraphRewriting.Graph.Write.Unsafe as GU
import qualified GraphRewriting.Graph.Read as G

import qualified GraphRewriting.Layout.Coulomb as GL
import qualified GraphRewriting.Layout.Gravitation as GL
import qualified GraphRewriting.Layout.Position as GL
import qualified GraphRewriting.Layout.SpringEmbedder as GL
import qualified GraphRewriting.Layout.Wrapper as GL

import qualified Data.Vector.V2 as V

data GraphType = GraphType ObjectRef [G.Port] [Bool]
instance G.View [G.Port] GraphType where
	inspect (GraphType _ ps _) = ps
	update ps (GraphType o _ bs) = GraphType o ps bs
	adjust f (GraphType o ps bs) = GraphType o (f ps) bs


newGraphType :: ObjectRef -> GraphType
newGraphType o = GraphType o [] []

graphOfPatch :: Patch -> G.Graph GraphType
graphOfPatch p = G.execGraph (nodes >>= edges) G.emptyGraph
	where
	nodes = mapM G.newNode (map newGraphType$ objectRefs p)
	edges ns = mapM (mkEdge ns) (connections p)
	mkEdge ns (Outlet (ObjectRef o,_), Inlet (ObjectRef i,_)) = do
		edge <- G.newEdge
		addEdge (ns!!o) True edge
		addEdge (ns!!i) False edge
	addEdge n b e = G.modifyNode n (\(GraphType o ps bs) -> GraphType o (e:ps) (b:bs))

layout :: G.Graph (GL.Wrapper GraphType) -> G.Graph (GL.Wrapper GraphType)
layout = G.execGraph$ do
	ns <- G.readNodeList
	mapM (const$ mapM layoutStep ns) [(1::Int)..10]

instance GL.PortSpec GraphType where
	portSpec (GraphType _ _ bs) =
		map (\dir->GL.sameDir$ V.Vector2 0 (if dir then 1 else -1)) bs

-- this function is from graph-rewriting-ski example use of graph-rewriting-layout
layoutStep n = do
	(cgf, cf, sf, rot) <- G.readOnly $ do
		cgf <- GL.centralGravitation n -- this is needed in order to prevent unconnected subgraphs to drift apart infinitely
		cf <- GL.coulombForce n        -- a repulsing force between each pair of nodes
		sf <- GL.springForce 1.5 n     -- this tries to push nodes into the position as dictated by the edges connected to other nodes.
		rot <- GL.angularMomentum n    -- this tries to rotate nodes such that the angular forces exercised by the edges upon the nodes are minimised.
		return (cgf, cf, sf, rot)
	-- combine all the forces specifying for each of them a function that maps the distance the force bridges to a strength.
	GU.adjustNode (GL.Position . sf (*0.9) . cgf (*0.01) . cf (\x -> min (50/(x^2+0.1)) 10) . GL.position) n

out :: Patch -> String
out p = unlines (prelude ++ outObjects p positions ++ outConnections p)
	where
	graph = layout (GL.wrapGraph (graphOfPatch p))
	positions = map (GL.inspect :: GL.Wrapper GraphType -> GL.Position) (G.nodes graph)

prelude :: [String]
prelude = ["#N canvas 0 0 100 100 pdgen;"]

outObjects :: Patch -> [GL.Position] -> [String]
outObjects p positions = map outObject (objects p `zip` positions)

outObject :: (Object,GL.Position) -> String
outObject (Object name args, v) = obj "obj" v (unwords (name:args))
outObject (Message msg, v) = obj "msg" v msg
outObject (FloatAtom, v) = obj "floatatom" v "5 0 0 0 - - -"
outObject (SymbolAtom, v) = obj "symbolatom" v "5 0 0 0 - - -;"

layoutMultiplier = 10
obj :: String -> GL.Position -> String -> String
obj ty (GL.Position v) rest = unwords ["#X", ty, show$ truncate$ V.v2x v * layoutMultiplier, show$ truncate$ V.v2y v * layoutMultiplier, rest, ";"]

outConnections :: Patch -> [String]
outConnections p = map outConnection (connections p)
outConnection :: (Outlet,Inlet) -> String
outConnection (Outlet (ObjectRef from, fromO), Inlet (ObjectRef to, toI)) = "#X connect " ++ unwords (map show [from, fromO, to, toI]) ++ ";"
