------------------------------------------------------------
-- Estructuras de Datos
-- Tema 3. Estructuras de Datos Lineales
-- Pablo López
--
-- Módulo del TAD Conjunto (para tipos ordenados)
------------------------------------------------------------

------------------------------------------------------------
-- Implementación del TAD Conjunto (para tipos ordenados)
------------------------------------------------------------

module Set(Set,
           empty,
           isEmpty,
           insert,
           isElem,
           delete,
           fold
          ) where

import           Test.QuickCheck

{-
   Al igual que las listas, pilas y colas, los conjuntos pueden
   definirse como una estructura de datos recursiva:

                     conjunto con 3 elementos
        elemento       /
              \  --------
            { 7, 3, 9, 5 }
             ------------
                     \
                     conjunto con 4 elementos

   Un conjunto de `n` elementos puede interpretarse como un
   elemento que se "inserta" en un conjunto de `n-1` elementos.
   El conjunto más pequeño que podemos construir es el vacío.

   Sin embargo, hay una diferencia importante entre los conjuntos
   y el resto de los TADs que hemos visto. En listas, pilas y colas
   hay un elemento que ocupa una posición distinguida (cabeza, cima,
   primero). El elemento que hemos seleccionado para ilustrar la
   recursividad del tipo conjunto es arbitrario; en lugar del 7
   podríamos haber escogido cualquier elemento del conjunto:

                conjunto con 3 elementos
                /
               /   elemento
              ------ / -----
            { 7, 3, 9, 5 }
             ---------------
                     \
                     conjunto con 4 elementos

   Además, el orden en que aparecen los elementos es irrelevante.
   Los siguientes conjuntos son equivalentes al anterior:

               { 5, 9, 3, 7 }
               { 3, 5, 7, 9 }

   Esto no ocurre en listas, pilas o colas, donde el orden de aparición
   es relevante.

   A pesar de estas diferencias, representamos los conjuntos en Haskell
   mediante un tipo algebraico parametrizado y recursivo `Set a` similar
   a los utilizados en pilas y colas.
-}

data Set a = Empty
           | Node a (Set a)
           deriving (Show, Eq)

{-
   Queremos utilizar el tipo algebraico `Set` arriba definido para
   representar conjuntos construidos por el cliente del TAD, tales
   como el siguiente:

        insert 7 (insert 9 (insert 3 (insert 9 (insert 5 empty))))

   Observa que al construir el conjunto los elementos no se han
   facilitado en orden y, además, uno de los elementos (9) aparece
   repetido.

   El implementador del TAD Conjunto debe decidir cómo representar el
   conjunto anterior. Por ejemplo, puede decidir representar los
   conjuntos apilando los elementos conforme se insertan:

        Node 7 (Node 9 (Node 3 (Node 9 (Node 5 Empty))))

   esto significa que la representación puede contener elementos
   repetidos.

   Otra posibilidad es que el implementador decida almacenar los
   elementos ordenados y sin repeticiones:

        Node 3 (Node 5 (Node 7 (Node 9 Empty)))

   Ambas representaciones son correctas, aunque obviamente no son
   equivalentes.

   ¿Qué ventajas e inconvenientes tiene cada una de estas
   representaciones internas? ¿Cómo afecta al usuario la elección?

   * Invariante de representación:

   Un invariante de representación de un TAD es una propiedad
   (booleana) sobre la representación **interna** del TAD.

   Este invariante debe ser cierto para todo valor del TAD:

     - antes de aplicar una operación pública del TAD
     - después de aplicar una operación pública del TAD

   sin embargo, el invariante puede ser falso durante la aplicación de
   una operación pública o al aplicar operaciones privadas.

   En la práctica, esto significa que todo valor del TAD que manipula
   el código del cliente satisface el invariante, pues ha sido
   construido por las operaciones públicas del TAD.

   La primera representación que hemos propuesto antes, basada en
   apilar elementos conforme se insertan:

        Node 7 (Node 9 (Node 3 (Node 9 (Node 5 Empty))))

   no satisface ningún invariante interesante.

   Sin embargo, la segunda representación:

        Node 3 (Node 5 (Node 7 (Node 9 Empty)))

   satisface un invariante muy importante: los elementos aparecen
   ordenados y sin repeticiones. Sin embargo, esta representación
   solo puede usarse si el tipo `a` de los elementos del
   conjunto está ordenado (`Ord a`).

   Es obligación del implementador del TAD asegurar que todos los
   valores del TAD satisfacen este invariante. Mantener este
   invariante supone un esfuerzo adicional, pero se ve recompensado
   porque simplifica la implementación de ciertas operaciones y puede
   mejorar la eficiencia.
-}

-- invariante de representación: ordenado y sin repeticiones
sample :: Set Char
sample = Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))

-- complejidad: O(1)
-- |
-- >>> empty
-- Empty
empty :: Set a
empty = Empty

-- complejidad: O(1)
-- |
-- >>> isEmpty empty
-- True
-- >>> isEmpty sample
-- False
isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- complejidad: O(n)
-- |
-- >>> isElem 'x' sample
-- False
-- >>> isElem 'f' sample
-- True
isElem :: Ord a => a -> Set a -> Bool
isElem x Empty = False
isElem x (Node y z) 
          | x == y = True
          |otherwise = isElem x z 

-- complejidad: O(n)
-- |
-- >>> insert 'x' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'x' (Node 'z' Empty))))
-- >>> insert 'a' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty
insert x (Node y s) 
     | x < y  = Node x (Node y s)
     | x == y = Node y s  
     | otherwise = Node y (insert x s)

-- complejidad: O(n)
-- |
-- >>> delete 'x' sample
-- Node 'a' (Node 'c' (Node 'f' (Node 'z' Empty)))
-- >>> delete 'f' sample
-- Node 'a' (Node 'c' (Node 'z' Empty))

delete :: Ord a => a -> Set a -> Set a
delete x Empty = Empty
delete x (Node y s)
     | x == y = s
     | otherwise = Node y (delete x s) 

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z = fun
 where
  fun Empty       = z
  fun (Node x s)  = f x (fun s)


{-
   La siguiente instancia de `Arbitrary` es para enseñar a QuickCheck
   a generar `Set` aleatorias. No hay que saber cómo hacerlo;
   siempre se facilita.
-}

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do
                xs <- listOf arbitrary
                return (foldr insert empty xs)
