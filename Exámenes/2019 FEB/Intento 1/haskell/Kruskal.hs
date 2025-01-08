----------------------------------------------
-- Estructuras de Datos.  2018/19
-- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
-- Escuela Técnica Superior de Ingeniería en Informática. UMA
--
-- Examen 4 de febrero de 2019
--
-- ALUMNO/NAME:
-- GRADO/STUDIES:
-- NÚM. MÁQUINA/MACHINE NUMBER:
--
----------------------------------------------

module Kruskal(kruskal, kruskals) where

import qualified DataStructures.Dictionary.AVLDictionary as D
import qualified DataStructures.PriorityQueue.LinearPriorityQueue as Q
import DataStructures.Graph.DictionaryWeightedGraph

kruskal :: (Ord a, Ord w) => WeightedGraph a w -> [WeightedEdge a w]
kruskal wg = kruskalAux dict pq []
    where
        dict = foldr f D.empty (vertices wg)
            where
                f vert ac = D.insert vert vert ac
        pq = foldr g Q.empty (edges wg)
            where
                g edge ac = Q.enqueue edge ac

kruskalAux :: (Ord a, Ord w) => D.Dictionary a a -> Q.PQueue (WeightedEdge a w) -> [WeightedEdge a w] -> [WeightedEdge a w]
kruskalAux dict pq solAc
    | Q.isEmpty pq = solAc
    | (representante org dict) /= (representante dest dict) = kruskalAux (D.insert (representante dest dict) org dict) (Q.dequeue pq) [WE org w dest] ++ solAc
    | otherwise = kruskalAux dict (Q.dequeue pq) solAc
    where
        (WE org w dest) = Q.first pq

representante :: (Ord a) => a -> D.Dictionary a a -> a
representante a dict
    | a == valOfa = a
    | otherwise = representante valOfa dict
    where
        Just valOfa = D.valueOf a dict

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined
