-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2OrdenSuperior where

import           Data.Char
import           Test.QuickCheck

{-
   Contenido:

     9. Orden superior sobre listas: map y filter
    10. Funciones como valores
    11. Lambda expresiones o funciones anónimas
    12. Secciones
    13. Parcialización
    14. Composición de funciones
-}

-- | 9. Orden superior sobre listas: map y filter
------------------------------------------------------------

{-
   Las siguientes funciones recursivas aplican una función a cada
   elemento de una lista.
-}

-- |
-- >>> cuadrado 5
-- 25

cuadrado :: Int -> Int
cuadrado x = x * x

-- |
-- >>> cuadrados [1..6]
-- [1,4,9,16,25,36]

cuadrados :: [Int] -> [Int]
cuadrados []     = []
cuadrados (x:xs) = cuadrado x : cuadrados xs

-- |
-- >>> ordinales "Haskell"
-- [72,97,115,107,101,108,108]

ordinales :: String -> [Int]
ordinales []     = []
ordinales (x:xs) = ord x : ordinales xs

{-
   Las funciones anteriores se parecen mucho y conceptualmente siguen
   el mismo esquema:

        lista de entrada:   [  x1,   x2,   x3 ...    xn ]
                               |     |     |         |
        lista de salida:   [ f x1, f x2, f x3, ... f xn ]

   Lo que varía de una función a otra es:

      - el tipo de las listas de entrada y salida
      - la función 'f'

   Podemos abstraer y parametrizar:

      - el tipo mediante polimorfismo
      - la función 'f' mediante orden superior (pasar la función como parámetro)
-}

     -- tipo función (paréntesis obligatorios)
     --      /
aplica :: (a->b) -> [a] -> [b] -- predefinida como map
aplica _ []     = []
aplica f (x:xs) = f x : aplica f xs

{-
   Las siguientes funciones recursivas seleccionan aquellos elementos de una
   lista que verifican una propiedad concreta 'p'.
-}

-- |
-- >>> esConsonante 'a'
-- False
-- >>> esConsonante 'b'
-- True

esConsonante :: Char -> Bool
esConsonante x = toUpper x `notElem` "AEIOU"

-- |
-- >>> consonantes "Haskell"
-- "Hskll"

consonantes :: String -> String
consonantes [] = []
consonantes (x:xs)
   | esConsonante x = x : consonantes xs
   | otherwise = consonantes xs

-- |
-- >>> pares [1..10]
-- [2,4,6,8,10]

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
  | even x = x : pares xs
  | otherwise = pares xs

{-
   De nuevo, las funciones anteriores se parecen mucho y
   conceptualmente siguen el mismo esquema:

        lista de entrada:   [  x1,   x2,   x3,  x4, ... xn ]
                                     |     |            |
        lista de salida:    [        x2,   x3       ... xn ]

   las diferencias son:

      - el tipo de ambas listas
      - la condición 'p' que deben verificar los elementos

   Podemos abstraer y parametrizar:

      - el tipo de ambas mediante polimorfismo
      - la condición 'p' mediante orden superior
-}

       --  predicado (función que devuelve Bool)
       --    /
filtra :: (a->Bool) -> [a] -> [a] -- predefinida como filter
filtra _ [] = []
filtra p (x:xs)
  | p x = x : filtra p xs
  | otherwise = filtra p xs

-- | 10. Funciones como valores
------------------------------------------------------------

{-
   Supongamos un tipo básico como Int.

   ¿Qué podemos hacer con un valor de tipo Int?
-}

-- almacenarlo en una variable inmutable (darle un nombre):

unEntero :: Int
unEntero = 5

-- almacenarlo en una estructura de datos:

listaDeEnteros :: [Int]
listaDeEnteros = [unEntero, 6, 1, 27]

-- pasarlo como parámetro o devolverlo como resultado de una función:

inc :: Int -> Int
inc x = x + 1

-- aplicarle ciertas operaciones específicas del tipo Int (+,*,<,...)

-- escribirlo literalmente; por ejemplo 5

{-
    Idea clave de la programación funcional:

       - las funciones son valores
       - podemos hacer con ellas las mismas cosas que con un valor de Int, Char, Bool,...
-}

-- almacenar función en una variable inmutable (darle un mombre):

unaFunc :: Int -> Int
unaFunc = inc

-- almacenar función en una estructura de datos:

listaDeFunciones :: [Int->Int]
listaDeFunciones = [unaFunc, abs, cuadrado, signum]

-- pasar una función como parámetro a una función (map o filter lo hacen):

twice :: (Int->Int) -> Int -> Int
twice f x = f (f x)

-- devolverla como resultado de una función:

elige :: Bool -> (Int->Int)
elige x = if x then inc
          else cuadrado

-- aplicarle la operación específica del tipo función; la aplicación de la función a un valor: f x

-- escribirla literalmente mediante lambda expresiones

-- | 11. Lambda expresiones o funciones anónimas
------------------------------------------------------------

{-
   Para escribir literalmente el valor de una función utilizamos
   lambda-expresiones, también llamadas funciones anónimas.
-}

-- |
-- >>> cubo 2
-- 8

cubo :: Integer -> Integer
cubo x = x * x * x

-- |
-- >>> cubos [1..6]
-- [1,8,27,64,125,216]

cubos :: [Integer] -> [Integer]
cubos xs = map cubo xs

{-
   Las lambda-expresiones son especialmente útiles para pasar una
   función como parámetro. Por ejemplo, podemos reescribir cubos
   usando una lambda expresión.
-}

cubosConLambda :: [Integer] -> [Integer]
cubosConLambda xs = map (\ x -> x * x * x) xs
                    --     /        \
                    -- parámetro  cuerpo

-- Ejercicio: Define las siguientes funciones usando map, filter y lambdas.

-- |
-- >>> duplica [1..5]
-- [2,4,6,8,10]

duplica :: [Int] -> [Int]
duplica xs = map (\ x -> 2*x) xs

-- |
-- >>> sigChar "Haskell"
--- "Ibtlfmm"

sigChar :: String -> String
sigChar xs = map (\ x-> chr(ord x +1)) xs

-- |
-- >>> múltiplosDeTres [1,2,3,4,5,6,7,8,9,10,12]
-- [3,6,9,12]

múltiplosDeTres :: [Int] -> [Int]
múltiplosDeTres xs = filter (\ x -> mod x 3 == 0) xs

-- | 12. Secciones
------------------------------------------------------------

{-
   Una sección es un operador binario que recibe un solo
   operando, que puede ser el izquierdo o el derecho; por ejemplo:

              (+3)

              (2*)

   La sección espera recibir el otro operando para aplicar el operador,
   por lo que una sección es una función unaria (de un solo argumento):

              (+3)  5

              (2*)  7

   Si el operador no es conmutativo, el orden de la sección importa:

              (^2)  5

              (2^)  5

   Una sección es realmente azúcar sintáctico para una lambda expresión:

            (@n) ---> \ x -> x @ n
            (n@) ---> \ x -> n @ x
-}

-- Ejercicio: define cubosConSeccion y duplicaConSeccion usando secciones.


cubosConSeccion :: [Integer] -> [Integer]
cubosConSeccion xs = map (^3) xs

-- |
-- >>> duplicaConSeccion [1..5]
-- [2,4,6,8,10]

duplicaConSeccion :: [Int] -> [Int]
duplicaConSeccion xs = map(*2) xs

-- | 13. Parcialización
------------------------------------------------------------

{-
   Recuerda que una sección es un operador binario que recibe un solo operando y
   devuelve una función unaria que espera el otro operando.

   Esa idea se puede generalizar a cualquier función, no solo a
   operadores binarios: una función de n argumentos puede ser invocada
   con k argumentos, siendo k <= n; es decir, se le puenden pasar menos
   argumentos de los que espera.

   Cuando k < n:

       1) se pasan los primeros k argumentos
       2) se devuelve una función que espera los n-k restantes argumentos
-}

f :: Int -> Int -> Int -> Int
f x y z = x + 2*y + 3*z

g :: Int -> Int
g = f 1 2 -- versión especializada de f: g z = 1 + 2*2 + 3*z

-- Ejercicio: Define usando parcialización una función `aprúebame` que sustituya
--            todos los supensos por aprobados.

-- |
-- >>> apruébame [4, 8.5, 2.5, 7.25, 9.1, 6, 10]
-- [5.0,8.5,5.0,7.25,9.1,6.0,10.0]

apruébame :: [Double] -> [Double]
apruébame xs = undefined 
   

-- | 14. Composición de funciones
------------------------------------------------------------

{-
   En Matemáticas se define la composición de funciones de la siguiente
   manera:

      f o g (x) = f (g(x))

   En Haskell también está definida la composición de funciones:

      (f . g) x = f (g x)

   Primero se aplica 'g' a 'x', y al resultado se le aplica 'f'.
   Gráficamente se comporta como un cauce (pipeline) de dos etapas:

                   ┌──────┐           ┌──────┐
     f(g(x)) <─────│  f   ├<── g x ───│  g   ├<────── x
                   └──────┘           └──────┘

   El operador . es asociativo: podemos construir cauces con cualquier
   número de etapas. Por ejemplo, la composición:

                             (f . g . h) x

   Se puede interpretar como un cauce de tres etapas:

                   ┌──────┐              ┌──────┐           ┌──────┐
   f(g(h x)) <─────│  f   ├<── h(g x) ───│  g   ├<── h x ───│  h   ├<────── x
                   └──────┘              └──────┘           └──────┘

-}

-- La funcion log es parcial, podemos hacerla total tomando
-- el valor absoluto de su argumento:

logTotal :: Double -> Double
logTotal x = log (abs x)  -- mediante llamadas anidadas

logTotal' :: Double -> Double
logTotal' x = (log . abs) x -- mediante composición de funciones

-- Ejercicio: Define la función `nextChar` usando composición de funciones.

-- |
-- >>> nextChar 'a'
-- 'b'
nextChar :: Char -> Char
nextChar x = chr (ord x + 1)   

{-
   Ejemplo de composición de funciones:

   El cifrado César consiste en reemplazar cada carácter de un mensaje
   en claro por el carácter que está 'n' posiciones más adelante en el
   alfabeto; por ejemplo, para n = 3 tenemos:

   alfabeto     : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   alfabeto + 3 : "DEFGHIJKLMNOPQRSTUVWXYZ[\]"

       cifrar      3  "ZAPATO"  --> "]DSDWR"
       descifrar   3  "]DSDWR"  --> "ZAPATO"
-}

desplaza :: Int -> Char -> Char
desplaza n x = (chr . (+n) . ord) x

cifrar :: Int -> String -> String
cifrar n xs = map (desplaza n) xs

descifrar :: Int -> String -> String
descifrar n xs = cifrar (-n) xs  -- es lo mismo que: map (desplaza (-n)) xs

prop_cifradoOK xs n = n >= 0 && n <= 25 ==> (descifrar n . cifrar n) xs == xs
