-------------------------------------------------------------------------------
-- Ford-Fulkerson Algorithm. Maximal flow for a weighted directed graph.
--
-- Student's name: Ángel Manuel Soria Gil
-- Student's group:Doble Grado Matemáticas e Ingeniería Informática
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.FordFulkerson where

import Data.List  ((\\))
import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.WeightedDiGraphBFT

maxFlowPath :: Path (WDiEdge a Integer) -> Integer
maxFlowPath ((E x w y):rest) = foldr f w rest
    where
        f (E x' w' y') solAc 
            | w' <= w   = w'
            | otherwise = solAc

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p edges 
    | not ((E x p y) `elemWithoutWeight` edges) = edges ++ [(E x p y)]
    | otherwise                                 = foldr f [] edges
    where 
        f (E org w dst) solAc 
            | x == org && y == dst && w + p == 0 = solAc
            | x == org && y == dst               = (E x (w+p) y):solAc
            | otherwise                          = (E org w dst):solAc

elemWithoutWeight :: (Eq a) => WDiEdge a Integer -> [WDiEdge a Integer] -> Bool
elemWithoutWeight (E x p y) list = foldr f False list
    where
        f (E org w dst) solAc
            | x==org && dst == y = True
            | otherwise          = solAc


updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges path p edges = foldr f edges path
    where
        f (E x w y) solAc = updateEdge x y p solAc

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p sol 
    | not ((E x p y) `elemAux` sol) = sol ++ [(E x p y)]
    | otherwise                     = foldr f [] sol
    where
        f (E org w dst) solAc
            | x == org && y == dst           = (E x (w+p) y):solAc
            | y == org && x == dst && w == p = solAc
            | y == org && x == dst && w <  p = (E x (p-w) y):solAc
            | y == org && x == dst && w >  p = (E y (w-p) x):solAc
            | otherwise                      = (E org w dst):solAc

elemAux :: (Eq a) => WDiEdge a Integer -> [WDiEdge a Integer] -> Bool
elemAux (E x p y) list = foldr f False list
    where 
        f (E org w dst) solAc 
            | org == x && dst == y = True
            | org == y && dst == x = True
            | otherwise            = solAc


addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows path p sol = foldr f sol path
    where
        f (E x w y) solAc = addFlow x y p solAc


fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst = ffaux g src dst []

ffaux :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer] -> [WDiEdge a Integer]
ffaux wdg src dst sol
    | (not.null) path = ffaux wdg' src dst sol'
    | otherwise       = sol
        where
            path = case bftPathTo wdg src dst of 
                Nothing -> []
                Just c  -> c 
            mf        = maxFlowPath path 
            edges     = weightedDiEdges wdg
            nedges    = updateEdges path (-mf) edges
            nnedges   = updateEdges (invert path) mf nedges
            wdg'      = mkWeightedDiGraphEdges (vertices wdg) nnedges 
            sol'      = addFlows path mf sol 

invert :: (Ord a) => Path (WDiEdge a Integer) -> Path (WDiEdge a Integer)
invert []               = []
invert ((E x p y):rest) = (invert rest) ++ [(E y p x)]


maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = foldr f 0 sol
    where 
        f (E x p y) solAc 
            | x == src  = p+solAc
            | otherwise = solAc  

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst set = maxFlowMinCutAux g src dst set set' 
    where 
        set' = (vertices g)\\set
        maxFlowMinCutAux g src dst set set'
            | src `elem` set  = suma1 - suma2
            | src `elem` set' = suma2 - suma1
                where
                    l1            = [(y,w) | x <- set, (y,w) <- (successors g x), y <- set']
                    l2            = [(y,w) | x <- set', (y,w) <- (successors g x), y <- set]
                    f (y,w) solAc = w + solAc
                    suma1         = foldr f 0 l1 
                    suma2         = foldr f 0 l2
                        
             



-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium = undefined

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks = undefined

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink = undefined
