------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo cliente del TAD Stack
------------------------------------------------------------

module StackClient where

{-
   El cliente de un TAD no conoce su representación física.  Para
   operar sobre el TAD el cliente solo puede utilizar las operaciones
   públicas de la interfaz del TAD. Para el caso del TAD `Stack` estas
   operaciones son `empty`, `push`, `pop`, `top`, `isEmpty`).

   Dado que todas las implementaciones del TAD `Stack` tienen la
   misma interfaz y las operaciones satisfacen las mismas propiedades
   (axiomas), el cliente puede usar cualquiera de las implementaciones
   (`LinearStack` o `StackOnList`). La única diferencia que
   podría notar el cliente es la eficiencia (tiempo de ejecución y
   cantidad de memoria usada).
-}

-- podemos elegir cualquier implementación del TAD Stack

-- import           DataStructures.Stack.LinearStack
-- import           DataStructures.Stack.StackOnList
import           Stack

--  Podemos construir cualquier `Stack` con los constructores `empty` y `push`.

pila1 :: Stack Int
pila1 = push 2 (push 5 (push 7 (push 11 empty)))

pila2 :: Stack Char
pila2 = push 'a' (push 'b' (push 'c' (push 'd' empty)))

pila3 :: Stack (String, Int)
pila3 = push ("neon", 10) (push ("hidrogeno", 1) (push ("oxigeno", 8) empty))

--  ¿Podemos usar `Node` y `Empty` para construir una pila más eficientemente?

-- No; no podemos si Stack.hs es un verdadero TAD
-- pila4 :: Stack Int
-- pila4 = Node 1 (Node 2 (Node 3 Empty))

-- La función `list2Stack` convierte una lista en una `Stack`:

-- |
-- >>> list2Stack [1..10]
-- LinearStack(1,2,3,4,5,6,7,8,9,10)
-- >>> list2Stack "Haskell"
-- LinearStack('H','a','s','k','e','l','l')
list2Stack :: [a] -> Stack a
list2Stack xs = foldr push empty xs

-- La función `size` calcula el número de elementos de una `Stack`:

-- |
-- >>> size pila1
-- 4
size :: Stack a -> Int
size s
  | isEmpty s = 0
  | otherwise = 1 + size (pop s)

-- Ejercicio: Define la función `stack2List` convierte una `Stack` en una lista.

-- |
-- >>> stack2List pila1
-- [2,5,7,11]
stack2List :: Stack a -> [a]
stack2List s 
    | isEmpty s = []
    | otherwise = (top s : stack2List (pop s))  
