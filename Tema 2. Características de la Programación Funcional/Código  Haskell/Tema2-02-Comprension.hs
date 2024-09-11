-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2Comprension where

import           Data.Char

{-
   Contenido:

     8. Listas por comprensión
-}

-- | 8. Listas por comprensión
------------------------------------------------------------

{-
   En Matemáticas es posible definir un conjunto por enumeración,
   indicando uno a uno sus elementos:

         {0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100}

   Una técnica más potente es la definición de conjuntos por
   comprensión, que permite construir nuevos conjuntos a partir de
   otros conjuntos:

         { x^2 / x pertenece a {0..10} }

    La definición por comprensión permite definir conjuntos infinitos:

         { x^2 / x pertenece a N }

    ¿Qué representa el siguiente conjunto?

         { (x,y) | (x,y) pertenece a (R,R), x^2 + y^2 <= r^2 }

     Similarmente, en Haskell podemos definir listas por comprensión,
     es decir, listas definidas a partir de otras listas.

     La sintaxis básica es:

         [ expresión | patrón <- lista ]

     donde `patrón <- lista` es un generador que visita uno a uno
     los elementos de una lista que ya existe para crear otra lista
     nueva.
-}

-- |
-- >>> cuadradosHasta10
-- [0,1,4,9,16,25,36,49,64,81,100]
cuadradosHasta10 :: [Integer]
cuadradosHasta10 = [ x^2 | x <- [0..10] ] -- x = 0, 1, 2, 3 .. 10
                         --    \
                         -- generador
{-
   A la derecha de `<-` en el generador puede aparecer cualquier patrón que
   sea adecuado para el tipo base de la lista:
-}

-- |
-- >>> primeros [("java", 1995), ("python", 1991), ("haskell", 1990)]
-- ["java","python","haskell"]
primeros :: [(a, b)] -> [a]
primeros xs = [ x | (x, _) <- xs ]
                 --   \
                -- patrón tupla

{-
   Los generadores pueden anidarse dando lugar a un producto
   cartesiano. El orden en que aparecen los generadores es
   significativo pues se generan listas, no conjuntos.
-}

-- |
-- >>> anidados1
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
anidados1 :: [(Integer, Char)]
anidados1 = [ (x,y) | x <- [1,2,3], y <- "abc" ]

-- |
-- >>> anidados2
-- [(1,'a'),(2,'a'),(3,'a'),(1,'b'),(2,'b'),(3,'b'),(1,'c'),(2,'c'),(3,'c')]
anidados2 :: [(Integer, Char)]
anidados2 = [ (x,y) | y <- "abc", x <- [1,2,3] ]

{-
   En una definición por comprensión también pueden aparecer guardas.
   Las guardas son expresiones booleanas permiten descartar o filtrar
   valores devueltos por un generador; solo se tienen en cuenta los valores
   devueltos por el generador que satisfacen la guarda:

           [ expresión | patrón <- lista, guarda ]
-}

-- |
-- >>> consonantesDe "Haskell y Java"
--- "Hskll y Jv"
consonantesDe :: String -> String
consonantesDe xs = [ x | x <- xs,  toLower x `notElem` "aeiou" ]
              --            \                   \
              --         generador            guarda

{-
   A veces en una compresión aparece repetida una expresión que se
   evalúa más de una vez. Por ejemplo, en la siguiente compresión
   `toLower x` se evalúa dos veces:
-}

ejemploSinLet :: String -> String
ejemploSinLet xs = [ toLower x | x <- xs,  toLower x `notElem` "aeiou" ]

{-
   La forma habitual de evitar este reevaluación en Haskell es usar una
   definición local con `where`. Sin embargo, esto no funciona porque
   la variable `x` es local a la compresión.

   Para evitar repetir el cálculo realizamos una definición local con
   `let` dentro de la comprensión:
-}

ejemploConLet :: String -> String
ejemploConLet xs = [ y | x <- xs,  let y = toLower x, y `notElem` "aeiou" ]
                                --       \
                                -- definición local

{-
   Ejercicio: define las siguientes funciones usando listas por compresión.
-}

-- |
-- >>> divisores 32
-- [1,2,4,8,16,32]
divisores :: Integer -> [Integer]
divisores x = [y | y <- [1..x], x `mod` y == 0]




-- |
-- >>> esPrimo 12345678
-- False
-- >>> esPrimo 31
-- True
esPrimo :: Integer -> Bool
esPrimo x   | divisores x == [1,x] = True
            | otherwise = False






-- |
-- >>> take 25 primos
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
primos :: [Integer]
primos = [x | x <- [2..], esPrimo x == True]

{-
   Ejercicio: define mediante una comprensión una función que devuelva
   todas las fichas de un dominó.
-}

-- |
-- >>> dominó
-- [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6),(4,4),(4,5),(4,6),(5,5),(5,6),(6,6)]
--dominó :: [(Int, Int)]
--dominó = [(x, y) | x <- [0..6], y <- [0..6]]
