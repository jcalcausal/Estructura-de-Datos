-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g 
    | isEmpty g = True
    | length (vertices g) == 1 = True
    | otherwise = aux vertices g
    where 
        aux [] = True
        aux (v:vs) = even (degree v g) && aux vs

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = aux (deleteEdge g (v,u))
    where
        aux g
            | (degree v == 0) && (degree u == 0) = delete v (delete u g)
            | (degree v == 0) = delete v g
            | (degree u == 0) = delete u g

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = undefined

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles xs (y:ys)  = undefined

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = undefined

-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g = undefined
