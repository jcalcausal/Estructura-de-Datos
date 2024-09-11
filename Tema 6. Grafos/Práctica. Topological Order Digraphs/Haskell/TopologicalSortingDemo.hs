-------------------------------------------------------------------------------
-- Topological Sorting of a Graph
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

import           DataStructures.Graph.DiGraph
import           TopologicalSortingDic

g1 :: DiGraph Int
g1 = mkDiGraphSuc [2,3,5,7,8,9,10,11] suc
  where
    suc 3  = [8,10]
    suc 5  = [11]
    suc 7  = [5,8]
    suc 8  = [9]
    suc 11 = [2,9,10]
    suc _  = []

g2 :: DiGraph Int
g2 = mkDiGraphSuc [0..12] suc
  where
    suc 0  = [1,2,3,5,6]
    suc 2  = [3]
    suc 3  = [4,5]
    suc 4  = [9]
    suc 6  = [4,9]
    suc 7  = [6]
    suc 8  = [7]
    suc 9  = [10,11,12]
    suc 11 = [12]
    suc _  = []

g3 :: DiGraph Int
g3 = mkDiGraphSuc [0..4] suc
  where
    suc 0 = [1]
    suc 1 = [2]
    suc 2 = [3]
    suc 3 = [4]
    suc 4 = [2]
