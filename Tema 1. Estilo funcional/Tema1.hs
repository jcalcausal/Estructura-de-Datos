-------------------------------------------------------------------
-- Estructuras de Datos
-- Grado en Ingeniería Informática, del Software y de Computadores
-- Tema 1. Introducción a la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema1 where

import           Data.Char

-- Esto es un comentario de línea, como // en Java

{-
    Esto es un comentario de bloque, como /* */ en Java

    Los comentarios de bloque se puede {- anidar -}
-}

{-
   Contenido:

     1. Características de Haskell
     2. Notación currificada
     3. Definición de funciones
     4. Condicionales
     5. Dualidad función/operador
     6. Evaluación de expresiones (ver transparencias)
     7. Tuplas
     8. Funciones monomórficas
     9. Funciones polimórficas
    10. Funciones sobrecargadas
    11. Funciones parciales
    12. Definiciones locales
    13. La regla de sangrado
-}

-- | 1. Características de Haskell
------------------------------------------------------------

{-
   Haskell = Puro + Tipado estático fuerte + Perezoso

   * Puro: los datos son inmutables (como las String de Java)

   * Tipado estático fuerte:
      - el tipo se establece en tiempo de compilación y no varía
      - no se pueden mezclar tipos incompatibles
      - no hay conversión implícita entre tipos (ni siquiera entre números)

   * Perezoso: sólo se evalúa lo necesario para obtener el resultado

     Ejemplos de pereza:

     La función `undefined` provoca un error, pero en los siguientes ejemplos
     no siempre es necesario evaluarla:

         [1,2,3,4]
         [1,2,undefined,4] -- error

         sum [1,2,3,4]
         sum [1,2,undefined,4] -- error

         length [1,2,3,4]
         length [1,2,undefined,4] -- funciona a pesar de undefined
-}

-- | 2. Notación currificada
------------------------------------------------------------

{-
   Notación currificada:

   En Matemáticas, los argumentos de una función se encierran entre
   paréntesis y se separan por comas. Es habitual que la multipicación se
   denote simplemente yuxtaponiendo los factores, sin que aparezca ningún operador.

       f(a,b) + c d

   En Haskell usamos notación currificada. La multiplicación se denota siempre por *,
   como es habitual. La aplicación de una función a sus argumentos se denota yuxtaponiendo
   la función y los argumentos, sin que aparezcan comas y paréntesis.

       f a b + c*d

   Sin embargo, si un argumento es compuesto, debe encerrarse entre paréntesis.

      f (a+1) b

   * Ejercicios de currificación

   Escribe las siguientes expresiones en notación currificada:
   Parámetro simple sin paréntesis, compuesto con paréntesis.

    f(x+y)     ----- > f (x+y) 

    f(x+1,y)   ----- > f (x+1) y

    f(2*x,y+1) ----- > f (2*x) (y+1)

    f(g(x))    ----- > f (g x)

    f(x,g(y))  ----- > f x (g y)

    g(f(x,y))  ----- > g (f x y) 

    g(x)+y     ----- > g x + y 

    f(x)*g(x)  ----- > f x * g x 

    f(x,y) + u*v   ----- > f x y + u*v

    max(max(x,y+1), max(z,-z))  ----- >  max (max x (y+1)) (max z (-z))

   * Ejercicios de descurrificación

   Escribe las siguientes expresiones currificadas en la notación habitual:

    f 2 + 3 - g x y + 7   ---> f(2) + 3 - g(x,y) + 7

    g (2+x) y * 6         ---> g(2+x, y)*6

    f g x                 ---> f(g,x)

    f (g x)               ---> f (g(x))

    f f f x               ---> f (f, f, x)

    max (abs (-7)) 6*x    ---> max((abs(-7)), 6) * x

    x f                   ---> x (f)
-}

-- | 3. Definición de funciones
------------------------------------------------------------

-- Partimos de ejemplos de funciones simples en C/C++/Java y las
-- definimos en Haskell.

-- |
-- >>> twice 3
-- 6

{-

   int twice(int x) {
      return x + x;
   }

-}

twice :: Int -> Int --Declaración
twice x = x + x 

-- |
-- >>> second 7 3
-- 3

{-

   int second(int x, int y) {
      return y
   }

-}


-- |
-- >>> square 5
-- 25

{-

   int square(int x) {
      return x * x;
   }

-}

square :: Integer -> Integer
square x = x * x

-- |
-- >>> pythagoras 2 5
-- 29

{-

   int pythagoras(int x, int y) {
      return square(x) + square(y);
   }

-}

pythagoras :: Integer -> Integer -> Integer
pythagoras x y = undefined

-- | 4. Condicionales
------------------------------------------------------------

{-
   El condicional if then else en Haskell:

      - es una expresión, no una sentencia
      - el else es obligatorio
      - los tipos de then y else deben coincidir
-}

-- |
-- >>> máximo 7 3
-- 7
-- >>> máximo 6 2
-- 6

máximo :: Integer -> Integer -> Integer -- predefinida como max
máximo x y = undefined

-- |
-- >>> máximoDeTres 7 9 2
-- 9
-- >>> máximoDeTres 17 6 12
-- 17

máximoDeTres :: Int -> Int -> Int -> Int
máximoDeTres x y z = undefined

-- |
-- >>> signo 6
-- 1
-- >>> signo (-4)
-- -1
-- >>> signo 0
-- 0

signo :: Integer -> Integer -- predefinida como signum
signo x = undefined

{-
   Además del if then else, Haskell soporta guardas (similar a las
   funciones por casos). A partir de 2 casos es mejor emplear guardas
   que anidar if then else.
-}

máximoDeTres' :: Int -> Int -> Int -> Int
máximoDeTres' x y z = undefined

signo' :: Integer -> Integer
signo' x = undefined

-- Las funciones Haskell pueden ser recursivas; es decir, invocarse a sí mismas

-- |
-- >>> factorial 5
-- 120
-- >>> factorial 0
-- 1

factorial :: Integer -> Integer
factorial x = undefined

-- | 5. Dualidad función/operador
------------------------------------------------------------

{-
   Una función binaria (div, max, ...) puede usarse como operador (infijo)
   si escribimos su nombre entre comillas simples invertidas (a la derecha de la
   tecla p)

     div x y == x `div` y
     max x y == x `max` y
-}

-- función max usada como operador: `max`
máximoDeTres'' :: Int -> Int -> Int -> Int
máximoDeTres'' x y z = x `max` y `max` z

{-
   Un operador binario (+, *, ...) se puede usar como función (prefijo)
   si escribimos su nombre entre parántesis

     x + y == (+) x y
-}

suma :: Int -> Int -> Int
suma x y = (+) x y

-- | 6. Evaluación de expresiones (ver transparencias)
------------------------------------------------------------

-- | 7. Tuplas
------------------------------------------------------------

{-
   El tipo tupla en Haskell:

   - las tuplas son como vectores
   - un valor de tipo tupla se escribe encerrando entre paréntesis sus componentes,
     separados entre sí por comas:

                  (exp_1, exp_2, ..., exp_n)

   - el número de componentes es fijo, n
   - cada componente exp_i puede ser de un tipo distinto
   - es un producto cartesiano
   - existe la tupla vacía: ()
   - no existe la tupla unitaria: (x)
-}

unaTupla :: (Bool, Int, Char, Double)
unaTupla = (True, 2 + 3, 'a', 3.1416)

tuplaAnidada :: (Int, (Char, Int))
tuplaAnidada = (1, ('A', 65))

-- utilidad de las tuplas: una función puede devolver varios resultados

-- |
-- >>> sucPred 5
-- (6,4)

sucPred :: undefined
sucPred = undefined

-- |
-- >>> códigosDe 'a'
-- (65,97)

códigosDe :: undefined
códigosDe = undefined

{-

   Ejercicios de tipado de tuplas:

   Suponiendo que las expresiones enteras son de tipo Int y las
   expresiones reales son de tipo Double, tipar las siguientes
   expresiones:


     (2 `div` 3, True, 'z')  ::

     ( 1 < 2, if 5 < 7 then 'a' else 'b', -1, (7, 'c')) ::

     (8, (), toLower 'A') ::

     (8, ('a'), toLower 'A') ::

     (8, (()), toLower 'A') ::

     ( (ord 'a', ord 'z') , (ord 'A', ord 'Z') ) ::
-}

-- | 8. Funciones monomórficas
------------------------------------------------------------

{-
   Una función es monomórfica si los tipos de todos sus argumentos y
   el tipo de su resultado son concretos (empiezan por mayúscula).

   Todas las funciones que hemos visto hasta el momento son monomórficas.
-}

-- Aquí tenemos otros ejemplos de funciones monomórficas.
-- La única diferencia significativa entre ellas es su tipo.

-- |
-- >>> primeroI (6,8)
-- 6

primeroI :: (Integer, Integer) -> Integer
primeroI (x,y) = x

-- |
-- >>> primeroC ('f','p')
-- 'f'

primeroC :: (Char, Char) -> Char
primeroC (x,y) = x

-- |
-- >>> primeroB ( 1 < 2, 2 > 3)
-- True

primeroB :: (Bool, Bool) -> Bool
primeroB (x,y) = x

-- | 9. Funciones polimórficas
------------------------------------------------------------

{-
   En Java podemos introducir un tipo genérico mediante <T>:

        public static <T> T identidad(T x) {
               return x;
        }

   donde T puede ser cualquier tipo Java.

   Lo que Java llama genericidad en Haskell se llama polimorfismo.

   Una función es polimórfica si el tipo de al menos uno de sus argumentos
   o el tipo de su resultado no son concretos, sino que puede ser
   cualquier tipo Haskell.

   En Haskell si un tipo empieza por minúscula es una variable de tipo.
   Suelen utilizarse las primeras letras del alfabeto: a, b, c, ...

        identidad :: a -> a
        identidad x = x
-}

identidad :: undefined -- predefinida como id
identidad = undefined

{-
   Las funciones monomórficas primeroI, primeroC, primeroB son
   idénticas entre sí, solo cambia el tipo.  Podemos abstraer el tipo
   mediante polimorfismo; es decir, introduciendo una variable de
   tipo.
-}

-- |
-- >>> primero (6,8)
-- 6
-- >>> primero ('f','p')
-- 'f'
-- >>> primero ( 1 < 2, 2 > 3)
-- True

primero :: undefined -- predefinida como fst
primero = undefined

-- |
-- >>> segundo (6,8)
-- 8
--
-- >>> segundo ('f','p')
-- 'p'
--
-- >>> segundo ( 1 < 2, 2 > 3)
-- False

segundo :: undefined -- predefinida como snd
segundo = undefined

-- | 10. Funciones sobrecargadas
------------------------------------------------------------

{-
   En Java puedo restringir un tipo genérico:

         T extends Comparable<T>

   En Haskell puedo restringir una variable de tipo:

         Ord a => a

   Ord es una clase Haskell semejante a la interfaz Comparable de Java.
-}

-- |
-- >>> sonSimétricos (6,8) (8,6)
-- True
-- >>> sonSimétricos ('a','t') ('s','a')
-- False

-- ambos componentes de la tupla tienen el mismo tipo
sonSimétricos :: undefined
sonSimétricos = undefined

-- |
-- >>> sonSimétricos' (True, 3) (3, True)
-- True

-- ambos componentes de la tupla pueden tener tipos distintos
sonSimétricos' :: undefined
sonSimétricos' = undefined

-- |
-- >>> estáOrdenada (6,8)
-- True
-- >>> estáOrdenada ('c','a')
-- False

estáOrdenada :: undefined
estáOrdenada = undefined

-- | 11. Funciones parciales
------------------------------------------------------------

{-
   Una función se dice parcial si no está definida para algún valor
   de sus argumentos.

   Una función se dice total si está definida para todos los posibles
   valores de sus argumentos.

   Si una función es parcial, podemos elevar una excepción cuando no
   esté definida.
-}

-- |
-- >>> inverso 2
-- 0.5
-- >>> inverso 0
-- *** Exception: inverso: división por cero

inverso :: Double -> Double  -- predefinida como recip
inverso = undefined

-- | 12. Definiciones locales
------------------------------------------------------------

-- Cálculo de las raíces de una ecuación de segundo grado: ax^2 + bx + c = 0

-- |
-- >>> raíces 2 7 1
-- (-0.14921894064178787,-3.350781059358212)
--
-- >>> raíces 2 4 2
-- (-1.0,-1.0)
--
-- >>> raíces 2 0 1
-- *** Exception: raíces: las raíces son complejas
--
-- >>> raíces 0 1 1
-- *** Exception: raíces: la ecuación no es de segundo grado

-- La siguiente definición repite cálculos y no es muy legible
raíces :: Double -> Double -> Double -> (Double, Double)
raíces a b c
  | a == 0 = error "raíces: la ecuación no es de segundo grado"
  | b*b-4*a*c < 0 = error "raíces: las raíces son complejas"
  | otherwise = ((-b + sqrt (b*b-4*a*c)) / (2*a),
                 (-b - sqrt (b*b-4*a*c)) / (2*a))

{-
   Podemos mejorar la modularidad, la legibilidad y la eficiencia
   introduciendo definiciones locales con where para aquellas
   expresiones que se repiten.
-}

raíces' :: Double -> Double -> Double -> (Double, Double)
raíces' a b c
  | a == 0 = error "raíces': la ecuación no es de segundo grado"
  | disc < 0 = error "raíces': las raíces son complejas"
  | otherwise = ((-b + raizDisc) / dosA,
                 (-b - raizDisc) / dosA)
  where
    disc = b*b - 4*a*c
    raizDisc = sqrt disc
    dosA = 2*a

-- | 13. La regla de sangrado
------------------------------------------------------------

{-
   En muchos lenguajes de programación se usan llaves {} para delimitar
   bloques (cuerpos de función, bucles, etc.).

   En Haskell no se usan llaves: el sangrado es significativo y
   determina la estructura del código.

   Una definición termina cuando se encuentra código que tiene el mismo
   o menos sangrado (o cuando se acabe el fichero).
-}

circArea :: Double -> Double
circArea r = pi*r^2

rectArea :: Double -> Double -> Double
rectArea b h = b*h

circLength :: Double -> Double
circLength r = 2*pi*r

cylinderArea :: Double -> Double -> Double
cylinderArea r h = 2*circ + rect
   where
        circ = circArea r
        l = circLength r
        rect = rectArea l h
