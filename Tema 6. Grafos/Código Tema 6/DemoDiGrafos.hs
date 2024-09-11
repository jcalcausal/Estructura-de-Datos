------------------------------------------------------------
-- Estructuras de Datos
-- Tema 6. Grafos
-- Pablo López
--
-- Digrafos en Haskell
------------------------------------------------------------

module DemoDiGrafos where

import           DataStructures.Graph.DiGraph
import           DataStructures.Graph.DiGraphBFT
import           DataStructures.Graph.DiGraphDFT

g1 :: DiGraph Int
g1 = mkDiGraphSuc [1,2,3,4] suc
         where
          suc 1 = [2]
          suc 2 = [3]
          suc 3 = [1,2,4]
          suc 4 = []

g1' :: DiGraph Int
g1' = mkDiGraphEdges [1,2,3,4] [ 1 :-> 2
                               , 2 :-> 3
                               , 3 :-> 1, 3 :-> 2, 3 :-> 4
                               ]
