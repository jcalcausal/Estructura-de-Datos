-------------------------------------------------------------------------------
-- Student's name: Ángel Manuel Soria Gil
-- Student's group: Doble Grado Matemáticas e Ingeniería Informática
--
-- Data Structures. Escuela Térnica Superior de Ingenieria Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, remove, extractCycle, connectCycles, vertexInCommon, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = foldr (&&) True boolList
    where 
        boolList = [x | v <- vertices g, let x = even (degree g v)] 


-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 [v0]
    where
        aux g v xs
            | v == next = (graphWithoutCycle , (next:xs))             -- si ya he llegado a cerrar el ciclo
            | otherwise = aux g next (next:xs)                        -- en otro caso sigo
                where
                    next              = head (successors g v)
                    graphWithoutCycle = remove g (v,next)
   
-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ys = ys
connectCycles xs (y:ys) = frsxs ++ (y:ys) ++ (tail scndxs)
    where
        frsxs  = takeWhile notequaly xs
        scndxs = dropWhile notequaly xs 
        notequaly x = x /= y

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = head commons
    where
        commons = [v | v <- vertices g, v `elem` cycle]

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
    | not(isEulerian g) = error "grafo no euleriano"
    | otherwise = eulerianCycle' g' ciclo
        where
            (g',ciclo) = extractCycle g (head (vertices g))

eulerianCycle' :: Eq a => Graph a -> Path a -> Path a 
eulerianCycle' g cycle
    | isEmpty g = cycle
    | otherwise = connectCycles cycle (eulerianCycle' g' xs)
        where
            (g',xs) = extractCycle g (vertexInCommon g cycle)

                    
-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u)
    | not(null lista) = mkGraphEdges lista (edges g)
    | otherwise       = deleteEdge g (v,u)
        where
            sinarista = deleteEdge g (v,u)
            lista     = [ v | v <- vertices g , degree (deleteEdge g (v,u)) v /= 0]




