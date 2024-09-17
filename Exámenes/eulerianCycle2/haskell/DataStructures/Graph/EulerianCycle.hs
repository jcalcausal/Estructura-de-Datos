-------------------------------------------------------------------------------
-- Student's name: Ángel Manuel Soria Gil
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática D
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = isEulerianAux (vertices g)
    where 
        isEulerianAux (v:rest) = even (degree g v) && isEulerianAux rest
        isEulerianAux []       = True 

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u)
    | degree g v <=1 && degree g u <=1 = deleteVertex (deleteVertex (deleteEdge g (v,u)) v) u
    | degree g v <= 1                  = deleteVertex (deleteEdge g (v,u)) v 
    | degree g u <= 1                  = deleteVertex (deleteEdge g (v,u)) u 
    | otherwise                        = deleteEdge g (v,u)

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = extractCycleAux g v0 v0 (head (successors g v0)) (g, [v0])
    where
        extractCycleAux g v0 v u (g', cycle) 
            | v0 == u   = ((remove g' (v,u)), cycle++[u])
            | otherwise = extractCycleAux g' v0 u (head (successors g' u)) ((remove g' (v,u)), cycle++[u]) 

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys      = ys
connectCycles xs (y:ys)  = xs1 ++ init (y:ys) ++ xs2
    where 
        f elem = y == elem
        xs1    = takeWhile (not.f) xs
        xs2    = dropWhile (not.f) xs 
        
lista1 = ['a', 'c', 'd', 'a']
lista2 = ['c', 'b', 'e', 'c']

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = head [v | v <- vertices g, v `elem` cycle]

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g 
    | isEmpty g    = []
    | isEulerian g = eulerianCycleAux g (head (vertices g)) []
    | otherwise    = error "applied eulerianCycle on not eulerianCycle"

eulerianCycleAux :: Eq a => Graph a -> a -> Path a -> Path a 
eulerianCycleAux g v path
    | isEmpty g = path
    | otherwise = eulerianCycleAux g' v' path'
    where
        path'   = (connectCycles path p)
        (g', p) = extractCycle g v 
        v'      = vertexInCommon g' path'

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
data Vertex = A | B | C | D | E  deriving (Show,Eq,Enum,Ord)

g5 :: Graph Vertex
g5 = mkGraphEdges [A .. E]
                  [(A, C), (A, D), (B, C), (B, E), (C, D), (C, E)]

g3 :: Graph Vertex
g3 = fst (extractCycle g5 A)