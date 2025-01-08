----------------------------------------------
-- Estructuras de Datos.  2018/19
-- 2º Curso del Grado en Ingeniería [Informática | del Software | de Computadores].
-- Escuela Técnica Superior de Ingeniería en Informática. UMA
--
-- Examen 4 de febrero de 2019
--
-- ALUMNO/NAME: Ángel Manuel Soria Gil
-- GRADO/STUDIES: Doble Grado Matemáticas e Ingeniería Infomática
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
        pq = foldr g Q.empty (edges wg)
         where
            g solAc edge = Q.enqueue edge solAc
        dict = foldr f D.empty (vertices wg)
         where f solAc vertex = D.insert vertex vertex solAc

kruskalAux :: (Ord a, Ord w) => D.Dictionary a a -> Q.PQueue (WeightedEdge a w) -> [WeightedEdge a w] -> [WeightedEdge a w]
kruskalAux d pq solAc 
    | Q.isEmpty pq     = solAc
    | not areConnected = kruskalAux(D.insert (representante dst d) org d (Q.dequeue pq)) [(WE org w dst)] ++ solAc 
    | otherwise        = kruskalAux d (Q.dequeue pq) solAc
    where
        (WE org w dst) = Q.first pq 
        areConnected = representante org d == representante dst d

representante :: (Ord a) => a -> D.Dictionary a a -> a 
representante v d
    | v == valOfV = v
    | otherwise = representante valOfV d 
    where
        Just valOfV = D.valueOf v d 

-- Solo para evaluación continua / only for part time students
kruskals :: (Ord a, Ord w) => WeightedGraph a w -> [[WeightedEdge a w]]
kruskals = undefined
-- se hace igual que kruskal solo que añadir en el caso en que es el mismo su propio representante 
-- (creo) y cambiar la salida para que sea lista de listas de aristas 
