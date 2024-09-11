------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo cliente del TAD Cola
------------------------------------------------------------

module QueueStackClient where

{-
   Dado que tanto el módulo `Stack` como el módulo `Queue` exportan la
   operación `empty`, es necesario utilizar una importación
   cualificada:

           import qualified NombreDeModulo as Prefijo

   Para hacer referencia a los elementos públicos del módulo necesitamos
   prefijarlos con `Prefijo`. Esto permite distinguir si nos estamos
   refiriendo al `empty` de `Stack` o al de `Queue`.
-}

import qualified Queue as Q
import qualified Stack as S

stack1 :: S.Stack Integer
stack1 = S.push 1 (S.push 2 (S.push 3 (S.push 4 S.empty)))

stackSize :: S.Stack a -> Integer
stackSize s
  | S.isEmpty s = 0
  | otherwise = 1 + stackSize (S.pop s)

queue1 :: Q.Queue String
queue1 = Q.enqueue "C++" (Q.enqueue "Java" (Q.enqueue "Haskell" Q.empty))

queueSize :: Q.Queue a -> Integer
queueSize q
  | Q.isEmpty q = 0
  | otherwise = 1 + queueSize (Q.dequeue q)

-- |
-- >>> list2Queue "hola"
-- Node 'h' (Node 'o' (Node 'l' (Node 'a' Empty)))
list2Queue :: [a] -> Q.Queue a
list2Queue xs = foldr Q.enqueue Q.empty (reverse xs)

-- Ejercicio: Define la función `queue2List` que convierte una `Queue` en una lista.

-- |
-- >>> queue2List queue1
-- ["Haskell","Java","C++"]
queue2List :: Q.Queue a -> [a]
queue2List q 
  | Q.isEmpty q = []
  | otherwise = Q.first q : queue2List (Q.dequeue q)