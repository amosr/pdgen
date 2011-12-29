{-# OPTIONS -XFlexibleInstances -XMultiParamTypeClasses #-}
module Language.Pd.PdGen.Out.Layout (layout) where

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

data GraphType = GraphType ObjectRef [G.Port] [Bool] Bool
instance G.View [G.Port] GraphType where
	inspect (GraphType _ ps _ _) = ps
	update ps (GraphType o _ bs pin) = GraphType o ps bs pin
	adjust f (GraphType o ps bs pin) = GraphType o (f ps) bs pin


newGraphType :: ObjectRef -> Bool -> GraphType
newGraphType o pin = GraphType o [] [] pin

pin (GraphType _ _ _ p) = p

graphOfPatch :: Patch -> G.Graph GraphType
graphOfPatch p = G.execGraph (nodes >>= edges) G.emptyGraph
	where
	nodes = mapM G.newNode (map (uncurry newGraphType)$ objectRefs p `zip` pins)
	pins = map isPin$ objects p

	isPin (Object "inlet" _) = True
	isPin (Object "inlet~" _) = True
	isPin (Object "outlet" _) = True
	isPin (Object "outlet~" _) = True
	isPin _ = False

	edges ns = mapM (mkEdge ns) (connections p)
	mkEdge ns (Outlet (ObjectRef o,_), Inlet (ObjectRef i,_)) = do
		edge <- G.newEdge
		addEdge (ns!!o) True edge
		addEdge (ns!!i) False edge
	addEdge n b e = G.modifyNode n (\(GraphType o ps bs pin) -> GraphType o (e:ps) (b:bs) pin)

layoutGraph :: G.Graph (GL.Wrapper GraphType) -> G.Graph (GL.Wrapper GraphType)
layoutGraph = G.execGraph$ do
	ns <- G.readNodeList
	mapM layoutInit (ns `zip` [0..])
	mapM (const$ mapM layoutStep (ns `zip` [0..])) [(1::Int)..1]

layoutInit (n,i) = do
	GU.adjustNode (GL.Position . const (V.Vector2 (fromIntegral$ i) (fromIntegral$ i)) . GL.position) n

instance GL.PortSpec GraphType where
	portSpec (GraphType _ _ bs _) =
		map (\dir->GL.sameDir$ V.Vector2 0 (if dir then 1 else -1)) bs

-- this function is from graph-rewriting-ski example use of graph-rewriting-layout
layoutStep (n,i) = do
	(cgf, cf, sf, rot) <- G.readOnly $ do
		cgf <- GL.centralGravitation n -- this is needed in order to prevent unconnected subgraphs to drift apart infinitely
		cf <- GL.coulombForce n        -- a repulsing force between each pair of nodes
		sf <- GL.springForce 1.5 n     -- this tries to push nodes into the position as dictated by the edges connected to other nodes.
		rot <- GL.angularMomentum n    -- this tries to rotate nodes such that the angular forces exercised by the edges upon the nodes are minimised.
		return (cgf, cf, sf, rot)
	-- combine all the forces specifying for each of them a function that maps the distance the force bridges to a strength.
	n' <- G.readNode n
	-- HACK: pin inlets and outlets so they are same left-to-right order as they are introduced
	if pin$ GL.wrappee n'
	 then GU.adjustNode (GL.Position . const (V.Vector2 (fromIntegral$ i) (fromIntegral$ i)) . GL.position) n
	 else GU.adjustNode (GL.Position . sf (*0.9) . cgf (*0.01) . cf (\x -> min (50/(x^2+0.1)) 10) . GL.position) n

layout :: Patch -> [(ObjectRef,(Int,Int))]
layout p = map get (G.nodes graph)
	where
	graph = layoutGraph (GL.wrapGraph (graphOfPatch p))
	get wrapper = let (GL.Position vec) = GL.inspect wrapper :: GL.Position  in
		case (GL.wrappee wrapper) of
		GraphType obj _  _ _ -> (obj,pos vec)
	pos (V.Vector2 x y) = (truncate x * layoutMultiplier, truncate y * layoutMultiplier)

layoutMultiplier = 30
