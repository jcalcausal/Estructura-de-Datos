-------------------------------------------------------------------------------
-- Ford-Fulkerson Algorithm. Maximal flow for a weighted directed graph.
--
-- Student's name: Ángel Manuel Soria Gil
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática D
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.FordFulkerson where

import Data.List  ((\\))
import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.WeightedDiGraphBFT

maxFlowPath :: Path (WDiEdge a Integer) -> Integer
maxFlowPath ((E org w dst):rest) = foldr f w rest 
    where
        f (E org' w' dst) solAc
            | w' >= solAc = w'
            | otherwise   = solAc

updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge org dst p edges 
    | (org, dst) `elemAux` edges = updateEdgeAux (E org p dst) edges
    | otherwise = (E org p dst):edges
    where
        elemAux (org, dst) ((E org' w dst'):rest) = (org == org' && dst == dst') || elemAux (org, dst) rest
        updateEdgeAux (E org p dst) edges = foldr f [] edges
            where
                f (E org' w' dst') solAc 
                    | org' == org && dst' == dst && w'+p == 0 = solAc
                    | org' == org && dst' == dst             = (E org (w'+p) dst):solAc
                    | otherwise                              = (E org' w' dst'):solAc


updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges path p edges = foldr f edges path
    where
        f (E org w dst) solAc = updateEdge org dst p solAc

addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow org dst p sol = foldr f [] sol
    where 
        f (E org' w dst') solAc 
            | org' == org && dst' == dst           = (E org (w+p) dst):solAc
            | dst' == org && p == w && org' == dst = solAc
            | dst' == org && p >  w && org' == dst = (E org (p-w) dst):solAc
            | dst' == org && p <  w && org' == dst = (E org (w-p) dst):solAc
            | otherwise                            = (E org p dst):solAc

addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows path p edges = foldr f edges path
    where
        f (E org w dst) solAc = addFlow org dst p solAc

fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst = aux g src dst [] (bftPathTo g src dst)
    where
        aux _ _ _ sol Nothing         = sol
        aux g src dst sol (Just path) = aux ng src dst (addFlows path mf sol) (bftPathTo ng src dst)
            where
                mf = maxFlowPath path
                ng = mkWeightedDiGraphEdges (vertices g) edges
                    where
                        reverseEdges = reversePath path
                        edges = updateEdges reverseEdges mf (updateEdges path (-mf) (weightedDiEdges g))
                        
reversePath :: Path (WDiEdge a Integer) -> Path (WDiEdge a Integer)
reversePath [] = []
reversePath ((E org w dst):rest) = reversePath rest ++ [E dst w org]


maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = foldr f 0 sol
    where
        f (E org w dst) solAc 
            | org == src = solAc + w
            | otherwise  = solAc

maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst k = undefined

{--
maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst k = aux g src dst k (k\\vertices g)
    where 
        aux g src dst k kp = (foldr f 0 kk) + (foldr g 0 kkp)
            where
                kk  = [(v, w) | vertex <- k, (v,w)<- successors vertex g]
                kkp = [(v, w) | vertex <- kp, (v,w)<- successors vertex g] 
                f (vertex, w) solAc 
                    | vertex `elem` kp = solAc + w
                    | otherwise     = solAc
                g (vertex, w) solAc
                    | vertex `elem` k = solAc - w 
                    | otherwise    = solAc 
--}



-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial 
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium = undefined

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks = undefined

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink = undefined
