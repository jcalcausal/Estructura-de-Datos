-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2Listas where

import           Data.Char
import           Data.List
import           Test.QuickCheck

{-
   Contenido:

     1. El tipo lista
     2. Secuencias aritméticas
     3. Funciones predefinidas sobre listas
     4. Constructores de listas
     5. Patrones para listas
     6. Recursividad sobre listas. Eficiencia
     7. Recursividad sobre listas con acumulador. Eficiencia

-}

-- | 1. El tipo lista
------------------------------------------------------------

{-
   Una lista se escribe como una secuencia de expresiones separadas
   por comas y encerradas entre corchetes:

                   [e1, e2, e3, ..., en]

   Por ejemplo:

                   [1, 2, 3, 4, 5]

                   [7, 1, -6]

                   ['a', 'b', 'c']

                   [1 < 2]

                   []

   El tipo de una lista se escribe entre corchetes:

                  ['a', 'b', 'c'] :: [Char]

                  [1 + 1 == 2, 'a' < 'b'] :: [Bool]

                  [True] :: [Bool]

   Diferencia con las tuplas:

         1. todos los elementos deben tener el mismo tipo
         2. la longitud de la lista puede variar (no afecta a su tipo)

   Las listas son instancia de las clases Eq y Ord siempre que el
   tipo de los elementos sea instancia de Eq y Ord. La comparación
   se hace en orden lexicográfico:

                  [1, 1+1, 3] == [1, 2, 3, 4]

                  [5, 7, 9] < [5, 7, 3]

   Las listas de tipo base Char se pueden escribir como cadenas:

                  ['a', 'b', 'c'] == "abc"

   Al tipo [Char] también se le llama String:

                  "abc" :: String
-}

{-
   Ejercicio: tipar las siguientes expresiones asumiendo que las expresiones
   numéricas son de tipo Int.

      [1,2,3] :: [Int]

      "1,2,3" :: String

      [ ('a', 8), ('t', 6) ] :: [(Char, Int)]

      [ (True) ] :: [(Bool)]

      ( [True] ) :: ([Bool])

      [ (False, '0'), (True, '1') ] :: [(Bool, Char)]

      ( [False, True], ['0', '1'] ) :: ([Bool], [Char])

      [ [], [2, 3] ] :: [ [] , [Int] ]

      [ ["a"] ] :: [[String]]

      [ ['a'] ] :: [String]

      [ () ] :: [()]

-}

-- | 2. Secuencias aritméticas
------------------------------------------------------------

{-
   Algunas listas están formadas por elementos que forman una progresión aritmética:
   partiendo de un primer elemento, cada elemento se obtiene del anterior sumando una
   cantidad fija llamada "diferencia". Por ejemplo, tomando 1 como diferencia tenemos:
-}

hasta10 :: [Int]
hasta10 = [1,2,3,4,5,6,7,8,9,10]

{-

   Podemos construir estas listas escribiendo solo su primer y último elementos separados
   por puntos suspensivos; por ejemplo:

       [1..10]

   Por defecto la diferencia de la progresión aritmética es 1. Si queremos que la diferencia
   sea distinta de 1, debemos facilitar tres elementos: el primero, el segundo y el último.
   Haskell calcula la diferencia restando el segundo elemento del primero. Por ejemplo, aquí la
   diferencia es 2:

      [1,3..10]
-}

-- |
-- >>> cuentaAtrás
-- [10,9,8,7,6,5,4,3,2,1,0]

cuentaAtrás :: [Int]
cuentaAtrás = [10, 9 .. 0]

{-
   El tipo Char también se puede utilizar en progresiones aritméticas.
-}

-- |
-- >>> alfabeto
-- "abcdefghijklmnopqrstuvwxyz"

alfabeto :: String
alfabeto = ['a' .. 'z']

{-
   Como en Matemáticas, las progresiones aritméticas pueden ser infinitas: basta
   omitir el último elemento.
-}

naturales :: [Integer]
naturales = [1..]

pares :: [Integer]
pares = [0,2 ..]

-- | 3. Funciones predefinidas sobre listas
------------------------------------------------------------

{-
   Las siguientes funciones están predefinidas y las podemos usar
   sin importarlas. Observa que algunas de estas funciones son totales
   y otras son parciales.

      null :: [a] -> Bool

      head :: [a] -> a

      last :: [a] -> a

      tail :: [a] -> [a]

      init :: [a] -> [a]

      take :: Int -> [a] -> [a]

      drop :: Int -> [a] -> [a]


       head              tail
        |   ------------------------------
       [x1, x2, x3, ............, xn-1, xn]
        ------------------------------  |
                    init               last

       -------------- + -----------------
            take              drop

      length :: [a] -> Int

      elem :: Eq a => a -> [a] -> Bool

      notElem :: Eq a => a -> [a] -> Bool

      reverse :: [a] -> [a]

      (++) :: [a] -> [a] -> [a]
-}

-- | 4. Constructores de listas
------------------------------------------------------------

{-
   Aunque escribamos las listas de esta manera:

                         [2, 3, 5, 7]

   esta notación es azúcar sintáctico; es decir, una notación que
   nos facilita el lenguaje para escribir algo de forma más concisa
   y legible, pero que realmente ya se podía escribir de forma primitiva
   en el lenguaje.

   En Haskell las listas se construyen a partir de dos constructores:

         1. la constante [], que representa la lista vacía

         2. el operador (:), que sirve para construir una lista a partir de:

                - su cabeza (primer elemento)
                - su cola (lista con el resto de sus elementos)

   El tipo de estos constructores es el siguiente:

             [] :: [a]
             (:) :: a -> [a] -> [a]

   Observa que el operador (:) permite diferenciar dos partes en la lista;
   su cabeza y su cola:

                         (cabeza : cola) :: [a]
                            |       |
                            a      [a]

   Todas las listas que hemos escrito hasta el momento se pueden reescribir
   de forma primitiva usando estos constructores, pero es más engorroso:

            [2] --> 2 : []

            [2, 3] --> 2 : (3 : [])

            [2, 3, 5] --> 2 : (3 : (5 : []))

            [2, 3, 5, 7] -> 2 : (3 : (5 : (7 : [])))

   Para simplifcar la sintaxis, el operador : asocia a la derecha, por lo que
   podemos escribir las anteriores listas sin paréntesis:

            [2, 3, 5, 7] -> 2 : 3 : 5 : 7 : []

   Obviamente, podemos combinar la notación primitiva con azúcar sintáctico:

            2 : [3,5,7]

   aunque normalmente escribimos un valor de tipo lista usando azúcar sintáctico.

   En realidad, : es un operador (función binaria) y sus operandos pueden ser
   cualquier expresión, siempre que sean del tipo adecuado:

           (5000 + 5) : reverse [1..10]

   Además, : es una función muy eficiente: su complejidad es O(1), es decir, no
   depende del tamaño de sus parámetros.

   La notación basada en constructores suele usarse para escribir patrones
   para listas.
-}

-- | 5. Patrones para listas
------------------------------------------------------------

{-
   Recuerda que en Haskell un parámetro formal es un patrón y que sirve
   para imponer condiciones sobre el valor que puede tomar la entrada
   (parámetro real).

   Los patrones se usan para definir funciones por casos; por ejemplo,
   para el caso de las tuplas:
-}

esOrigen :: (Int, Int) -> Bool
esOrigen (0,0) = True  -- patrón tupla y dentro patrones constante
esOrigen _     = False -- patrón subrayado

{-
   También podemos usar patrones para las listas.

   El patrón de listas más simple es [], que encaja solo con la lista vacía.
-}

-- |
-- >>> estáVacía []
-- True
-- >>> estáVacía [1,2,3]
-- False

estáVacía :: [a] -> Bool -- predefinida como null
estáVacía [] = True  -- patrón lista vacía
estáVacía _  = False -- patrón subrayado (cualquier lista)

{-
   Cuando conocemos el número de elementos que tiene una lista, podemos
   utilizar un patrón basado en el azúcar sintáctico; por ejemplo:

                [patrón_1, patrón_2, ..., patrón_n]

   es un patrón que encaja solo con listas que tienen EXACTAMENTE n elementos.
-}

-- |
-- >>> esUnitaria [5]
-- True
-- >>> esUnitaria [5,1]
-- False

esUnitaria :: [a] -> Bool
esUnitaria [_] = True  -- patrón lista de exactamente un elemento
esUnitaria _   = False -- patrón subrayado (cualquier lista)

-- |
-- >>> suma3 [1,2,3]
-- 6
-- >>> suma3 [1,2]
-- *** Exception: suma3: la lista no tiene 3 elementos

suma3 :: [Int] -> Int
suma3 [x,y,z] = x + y + z -- patrón lista con exactamente 3 elementos
suma3 _       = error "suma3: la lista no tiene 3 elementos" -- cualquier otra lista

{-
   En la práctica es muy habitual que no sepamos cuántos elementos
   tiene la entrada (parámetro formal). En estos casos utilizamos patrones
   inspirados en los constructores de listas:

               (patrón_1 : patrón_2 : ... : patrón_n : patrón_cola)

   es un patrón que encaja solo con listas que tienen AL MENOS n elementos.
-}

-- |
-- >>> cabeza [3,1,2]
-- 3
-- >>> cabeza "haskell"
-- 'h'
-- >>> cabeza []
-- *** Exception: cabeza: lista vacía

cabeza :: [a] -> a -- predefinida como head
cabeza (x:_) = x -- patrón lista de al menos un elemento (i.e., no vacía)
cabeza _     = error "cabeza: lista vacía" -- aquí la lista solo puede estar vacía

-- |
-- >>> cabezaAlFinal [1,2,3]
-- [2,3,1]
-- >>> cabezaAlFinal []
-- *** Exception: cabezaAlFinal: lista vacía

cabezaAlFinal :: [a] -> [a]
cabezaAlFinal (x:xs) = xs ++ [x] -- patrón lista de al menos un elemento (i.e., no vacía)
cabezaAlFinal _      = error "cabezaAlFinal: lista vacía" -- aquí la lista solo puede estar vacía

-- |
-- >>> suma3Primeros [1,2,3,4,5,6,7,8,9,10]
-- 6
-- >>> suma3Primeros [1,2]
-- *** Exception: suma3Primeros: la lista tiene menos de 3 elementos

suma3Primeros :: [Int] -> Int
suma3Primeros (x:y:z:_) = x + y + z -- patrón lista con al menos 3 elementos
suma3Primeros _         = error "suma3Primeros: la lista tiene menos de 3 elementos"

-- | 6. Recursividad sobre listas
------------------------------------------------------------

{-
   * 6.1 Las listas son una estructura recursiva

   Recuerda que las listas se construyen con dos constructores:

            []       - lista vacía
            x : xs   - lista no vacía con cabeza y cola

   Observa que la cola de una lista es también una lista: una lista
   de longitud n está formada por una cabeza seguida por una lista
   de longitud n-1; por ejemplo:

                           lista de longitud 5
                                  |
                     --------------------------
                     2 : (3 :  5 : 7 : 11 : [])
                    /    ----------------------
                cabeza           |
                               cola
                          lista de longitud 4

   Esto quiere decir que la lista es una estructura de datos recursiva:
   una lista contiene una lista que a su vez contiene una lista,...
   El caso base es la lista vacía [], que como su nombre indica no
   contiene nada.

   * 6.2 Proceso recursivo de listas

   Dado que las listas son recursivas, es habitual resolver problemas
   sobre listas aplicando recursión. Por ejemplo, si queremos calcular
   la suma de los elementos de una lista:

                       suma [2,3,5,7,11]

   calculamos primero la solución para la cola (que tiene un elemento
   menos):

                        suma [2,3,5,7,11]
                                --------         solución de la cola
                                   |            /
                          suma [3,5,7,11]  =  26

   Una vez que conocemos la solución de la cola, solo tenemos que tener
   sumar la cabeza a la suma de la cola:

                                    sumar cabeza   solución total
                                              \          /
                        suma [2,3,5,7,11]  =  2 + 26 = 28
                                --------           \
                                |                  solución de la cola
                                |                /
                          suma [3,5,7,11]  =  26

   En cada paso de la recursión la longitud de la lista a procesar
   disminuye en 1. El proceso recursivo de listas finaliza cuando alcanzamos
   el caso base: la lista vacía.

   La definición de la función suma es la siguiente:

-}

-- |
-- >>> suma [2,3,5,7,11]
-- 28

suma :: [Integer] -> Integer -- predefinida como sum
suma []     = 0              -- caso base
suma (x:xs) = x + suma xs    -- caso recursivo

-- Completa las siguientes funciones recursivas sobre listas

-- |
-- >>> longitud [1,2,3]
-- 3
-- >>> longitud []
-- 0
-- >>> longitud "haskell"
-- 7

longitud :: [a] -> Integer -- predefinida como length
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- |
-- >>> inversa "abc"
-- "cba"

inversa :: [a] -> [a] -- predefinida como reverse
inversa [] = []
inversa [x] = [x]
inversa (x:xs) = (inversa xs) ++ [x] 

-- |
-- >>> ordenada [-6, 4, 17]
-- True
--
-- >>> ordenada "java"
-- False

ordenada :: Ord a => [a] -> Bool
ordenada [] = True
ordenada [_] = True
ordenada (x:y:xs) = x<=y && ordenada xs

{-

   * 6.3 Definición de funciones recursivas sobre listas

   Podemos resumir todo el proceso recursivo en el siguiente esquema:

                f (x:xs) --------- "añadir" x a la solución de la cola
                   |                    |
                 f xs    --------- solución de la cola
                   .                    .
                   .                    .
                   .                    .
                 f []    --------- solución caso base


   Las funciones recursivas sobre listas se definirán por casos y
   tendrán al menos dos casos:

      - un caso base: suele ser la lista vacía y tener solución inmediata

      - un caso recursivo: suele ser la lista no vacía (con cabeza y cola)
        y "añade" la cabeza a la solución de la cola, que se calcula en la
        llamada recursiva

   Estos dos casos se pueden expresar en Haskell con dos ecuaciones con
   patrones:

           f []     = solución caso base

           f (x:xs) = añadir x (f xs)
                                 \
                          solución de la cola
                          (llamada recursiva)

   Este esquema se puede aplicar de manera casi mecánica al definir funciones
   recursivas. La parte creativa consiste en determinar para cada problema:

       - ¿cuál es la solución base?
       - ¿cómo se "añade" la cabeza a la solución de la cola?

   Sin embargo, es muy importante entender que no todos los problemas de recursión
   sobre listas se ajustan al esquema anterior (por ejemplo, la función ordenada).

   La mayoría de las estructuras de datos que estudiaremos son recursivas
   y, por consiguiente, la mayoría de las funciones que las procesan serán
   también recursivas.
-}

-- eficiencia: conteo de reducciones (ver transparencias)
-- eficiencia de length
-- eficiencia de ++
-- eficiencia de inversa

-- | 7. Recursividad con acumulador
------------------------------------------------------------

{-
   * 7.1 Recursión con acumulador

   La recursión sobre listas funciona calculando la solución para la
   cola:

                problema [3,5,7,11,13]
                            ---------
                               |
                       solución de la cola

   Una vez que hemos calculado la solución de la cola, "añadimos" la
   cabeza para obtener la solución de toda la lista.

   También es posible resolver los problemas con un esquema alternativo
   que no se "salta" elementos y después los "añade"; sino que va
   calculando la solución de los elementos que ya se han visitado.

   Por ejemplo si queremos calcular la suma de los elementos de una lista:

                    suma [3,5,7,11,13]

   después de haber visitado los dos primeros elementos la situación es
   la siguiente:

                     suma [7,11,13]     8
                                        |
                          solución de los visitados [3,5]

   Cada vez que visitamos un elemento, lo "añadimos" a la solución de los
   visitados. Por ejemplo, al visitar el tercer elemento obtenemos:

                                  solución de los visitados [3,5,7]
                                                 |
                     suma [11,13]       8 + 7 = 15
                                       /     \
                                      /   añadir recién visitado
                                     /
                   solución de anteriormente visitados [3,5]

   Obviamente, cuando alcanzamos la lista vacía (el caso base), la
   solución de los visitados es la solución para toda la lista.

   Queda aún una cuestión por decidir: ¿cuánto vale la solución de los
   visitados cuando aún no hemos visitado ningún elemento? La solución
   es simple: debe ser la solución para la lista vacía.

   Para almacenar la solución de los elementos visitados utilizamos
   un acumulador; por ello a este esquema recursivo se le llama recursión
   con acumulador. Por tanto el esquema utiliza dos parámetros:

      - la lista sobre la que se aplica la recursión
      - un acumulador con la solución del prefijo de los elementos visitados

   Para el ejemplo de la suma de elementos de una lista, la recursión con
   acumulador procede de la siguiente manera:

                  xs                    ac

          suma [3,5,7]                  0  (solución para [])
          suma   [5,7]                  3  (añadir 3 al acumulador)
          suma     [7]                  8  (añadir 5 al acumulador)
          suma      []                 15  (añadir 7 al acumulador)

   Al llegar a la lista vacía, el acumulador contiene la solución.

   Podemos resumir todo el proceso recursivo en el siguiente esquema:

                f (x:xs) ---------   inicializar acumulador
                   |                           |
                 f xs    ---------   "añadir" x a acumulador
                   .                           .
                   .                           .
                   .                           .
                 f []    --------- el acumulador contiene la solución

   * 7.2 Definición de funciones recursivas con acumulador

   Las funciones recursivas con acumulador sobre listas son uno poco
   más laboriosas de definir porque esta recursión debe manejar dos
   argumentos: la lista y el acumulador.

   El punto de entrada a una función recursiva con acumulador es una
   función 'f' que solo recibe la lista y se limita a invocar a la
   función recursiva 'fRecAc' con el acumulador inicializado:

                     valor inicial del acumulador
                         (solución para [])
                              /
           f xs = fRecAc xs ac0
                    \
            función recursiva con acumulador

   La función recursiva con acumulador 'fRecAc' suele definirse de manera
   local a 'f' y es una función recursiva con dos casos:

      - un caso base: suele ser la lista vacía y la solución ya está en el acumulador

      - un caso recursivo: suele ser la lista no vacía (con cabeza y cola)
        y "añade" la cabeza al acumulador; la llamada recursiva recibe la cola
        de la lista y el acumulador con la cabeza añadida

   Estos dos casos se pueden expresar en Haskell con dos ecuaciones con  patrones:

           f xs = fRecAc xs ac0
              where                la solución está en el acumulador
                                  /
                 fAc []     ac = ac

                 fAc (x:xs) ac = fAc xs (añadir x ac)
                                              \
                                     acumulador actualizado

   Este esquema se puede aplicar de manera casi mecánica al definir funciones
   recursivas con acumulador. La parte creativa consiste en determinar para cada
   problema:

       - ¿cuál es la solución base?
       - ¿cómo se "añade" la cabeza al acumulador?

   Sin embargo, es muy importante entender que no todos los problemas de recursión
   sobre listas con acumulador se ajustan al esquema anterior.

   La recursión con acumulador no es natural como la recursión sobre listas, pero
   en algunos casos puede ser más eficiente (ver transparencias).
-}

suma' :: [Integer] -> Integer
suma' xs = sumaAc xs 0
   where
      sumaAc [] ac = ac
      sumaAc (x:xs) ac = sumaAc xs (x+ac)

-- |
-- >>> longitud' [1,2,3]
-- 3
--
-- >>> longitud' []
-- 0
--
-- >>> longitud' "haskell"
-- 7

longitud' :: [a] -> Int -- predefinida como length
longitud' xs  = longitudAc xs 0
   where
      longitudAc [] ac = ac
      longitudAc (x:xs) ac = longitudAc xs (1+ac)

-- |
-- >>> inversa' "abc"
-- "cba"

inversa' :: [a] -> [a] -- predefinida como reverse
inversa' xs = inversaAc xs []
   where 
      inversaAc [] ac = ac
      inversaAc (x:xs) ac = inversaAc xs (x:ac)