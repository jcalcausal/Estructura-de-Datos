

import qualified DataStructures.Graph.Graph as G
import qualified DataStructures.Dictionary.AVLDictionary as D
import Data.Maybe


createDict :: (Ord a) => G.Graph a -> D.Dictionary a Double
createDict g = foldr (`D.insert` 0) D.empty (G.vertices g) 

threshold :: Double
threshold = 0.00001

g1 :: G.Graph Char
g1 = G.mkGraphEdges ['A','B','C'] [('A','B'),('B','C'),('C','A')]

distribute :: Ord a => Double -> a -> G.Graph a -> D.Dictionary a Double 
distribute i vertex graph = distributeaux i vertex graph (createDict graph)

distributeaux :: Ord a => Double -> a -> G.Graph a -> D.Dictionary a Double -> D.Dictionary a Double
distributeaux i vertex graph dict 
    | i > threshold = distributelist (G.successors graph vertex) evenly graph (D.insert vertex (val+half) dict)
    | otherwise = dict
        where
            half = i*0.5
            Just val = (D.valueOf vertex dict)
            evenly = half / (fromIntegral (G.degree graph vertex))

distributelist :: Ord a => [a] -> Double -> G.Graph a -> D.Dictionary a Double -> D.Dictionary a Double
distributelist [] i g d = d 
distributelist (x:xs) i g d = distributeaux i x g (distributelist xs i g d)

erank :: Ord a => Double -> G.Graph a -> D.Dictionary a Double
erank i graph = distributelist (G.vertices graph) i graph (createDict graph)
