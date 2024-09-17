-------------------------------------------------------------------
-- Estructuras de Datos
-- Grado en Ingeniería Informática, del Software y de Computadores
-- Tema 1. Introducción a la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Punto where

{-
   Representaremos un punto en el plano por sus coordenadas cartesianas
   almacenadas en una tupla de dimensión 2. Por ejemplo:
-}

unPunto :: (Int, Int)
unPunto = (5, 2)

-- Completa la siguiente función sobre puntos:

-- |
-- >>> esOrigen (0,0)
-- True
-- >>> esOrigen (1,0)
-- False

esOrigen :: (Int, Int) -> Bool
esOrigen = undefined


{-
   Paso de parámetros y patrones:

   En lenguajes como Java/C/C++ el parámetro formal (el que aparece en
   la definición de la función) es una variable, mientras que el
   parámetro real (el que aparece en la llamada) es una expresión:


               Definición                                Invocación

             parámetro formal                            parámetro real
            (es una variable)                          (es una expresión)
                    |                                          |
                    |                                          |
          int f(int x) {                                   f(3 * 5)
              .....
          }


   En estos lenguajes el paso de parámetros consiste en inicializar el parámetro
   formal con el valor del parámetro real (x = 15). Es una operación incondicional
   que no puede fallar.

   En Haskell el parámetro real es una expresión y el parámetro formal es un
   patrón (no necesariamente una variable):

               Definición                                Invocación

             parámetro formal                            parámetro real
              (es un patrón)                           (es una expresión)
                   |                                           |
                   |                                           |
                 f x = ...                                f (3 * 5)


   En Haskell el paso de parámeros se realiza por correspondencia de patrones:
   el patrón (parámetro formal) puede imponer una restricción sobre el valor
   que puede tomar el parámetro real. Esto significa que el paso de parámetros
   está condicionado y puede fallar:

      1. Si el parámetro real encaja con el patrón, el paso de parámetros
         tiene éxito y las variables del patrón se inicializan al valor
         correspondiente del parámetro formal

      2. Si el parámetro real no encaja con el patrón, el paso de parámetros
         falla y no se puede realizar

   Los patrones disponibles para restringir los valores del parámetro real dependen
   del tipo del parámetro formal.
-}

{-
   En la función 'esOrigen' el parámetro formal es de tipo tupla. Veremos varios
   patrones para restringir el valor de la tupla de entrada.
-}

-- | Opción 1: patrón variable

{-
   El patrón es simplemente una variable y, como en Java/C/C++, no impone ninguna
   restricción sobre el valor de la entrada: acepta cualquier valor del tipo (Int, Int).
-}

esOrigen1 :: (Int, Int) -> Bool
esOrigen1 x = fst x == 0 && snd x == 0 -- patrón variable

{-
   El inconveniente de esta solución es que dado que 'x' es una tupla, tenemos que
   utilizar las funciones 'fst' y 'snd' para acceder a sus componentes. La mayoría
   de las funciones que hemos definido hasta ahora usan solo patrones variable.
-}

-- | Opción 2: patrón tupla

{-
   El patrón es una tupla y, a su vez, cada componente de la tupla se describe mediante
   un patrón: (patrón_1, patrón_2)
-}

esOrigen2 :: (Int, Int) -> Bool
esOrigen2 (x,y) = x == 0 && y == 0 -- patrón tupla

{-
   En esta ocasión tenemos un patrón tupla '(x,y)' y cada componente
   es un patrón variable, 'x' e 'y'. Esto significa que la función
   acepta como entrada cualquier valor del tipo (Int,Int). La ventaja
   respecto a la opción 1 es que podemos acceder a las componentes de la tupla
   sin necesidad de usar las funciones 'fst' y 'snd'.
-}

-- | Opción 3: patrón constante

{-
   El patrón es un valor concreto descrito mediante una constante. Es el patrón
   más restrictivo de todos: el parámetro real debe tomar exactamente el valor
   de esa constante; de lo contrario no encaja y el paso de parámetros fracasa.

   Cuando utilizamos patrones constantes es frecuente que las funciones se
   definan por casos: cada caso se describe en una ecuación independiente.
   Haskell trata de aplicar estas ecuaciones en el orden de aparición: si una
   ecuación no se puede aplicar (porque el patrón no encaja), se prueba con la
   siguiente. Solo se aplica la primera ecuación cuyo patrón encaja. Si ninguna
   ecuación es aplicable, el programa eleva una excepción.
-}

esOrigen3 :: (Int, Int) -> Bool
esOrigen3 (0, 0) = True   -- primero se prueba la primera ecuación
esOrigen3 x      = False  -- solo si la primera ecuación falla, se prueba la segunda

{-
   En esta ocasión tenemos en la primera ecuación un patrón tupla
   '(0,0)', cada componente es un patrón constante '0'. Por lo tanto,
   esa ecuación solo se puede aplicar si la entrada tiene el valor
   '(0,0)'. Esto no significa que la tupla de entrada deba ser
   literalmente '(0,0)'; también se puede aplicar a cualquier tupla
   cuyo valor sea '(0,0)', por ejemplo '(1-1, x * 0)'.
   Si la primera ecuación no es aplicable se intenta aplicar la segunda.
   En esta ocasión la segunda ecuación siempre es aplicable, porque
   utiliza un patrón variable que acepta cualquier valor del tipo '(Int,Int)'
-}

-- | Opción 4: patrón subrayado

{-
   El patrón subrayado se escribe '_' y es como el patrón variable (acepta
   cualquier valor de entrada), excepto que no le da ningún nombre, por lo que
   no se puede utilizar en la definición de la función (a la derecha del '=').
-}

esOrigen4 :: (Int, Int) -> Bool
esOrigen4 (0, 0) = True
esOrigen4 _      = False  -- patrón subrayado

{-
   Observa que la segunda ecuación no hace uso del parámetro: no necesitamos ni
   darle un nombre ni acceder a sus componentes: solo necesitamos saber que
   está ahí.
-}

{-
   Resumen de patrones:

   Los patrones no solo se aplican a las tuplas: se aplican a todos los tipos
   Haskell. A continuación resumimos los patrones que hemos estudiado.

        Patrón                            Significado

         x                        patrón variable, acepta cualquier valor y le da un nombre

       (patrón_1,...,patrón_n)    patrón tupla de dimensión n, permite imponer restricciones
                                  sobre los componentes y acceder a los mismos

         n                        patrón constante, solo acepta el valor literal de la constante

         _                        patrón subrayado, acepta cualquier valor pero lo de da nombre
-}

-- Ejercicios. Define las siguientes funciones usando patrones.

-- |
-- >>> espejo (4, 3)
-- (3, 4)

espejo :: (Int, Int) -> (Int, Int)
espejo = undefined

{-
   En el Tema 1 introducimos la función 'error' para señalar situaciones
   en las que una función parcial no está definida.
-}

inverso :: Double -> Double
inverso x
   | x == 0 = error "inverso: el 0 no tiene inverso" -- cadena de caracteres
   | otherwise = 1 / x

{-
   La función 'error' recibe una cadena de caracteres como parámetro.
   En Haskell, el tipo de las cadenas de caracteres se llama 'String'
   (lo estudiaremos en el Tema 2).

   La siguiente función 'paridad' devuelve una String.
-}

paridad :: Int -> String
paridad x = if even x then "es par" -- even y odd están predefinidas
            else "es impar"

-- |
-- >>> estaEn (0,0)
-- "en el origen"
-- >>> estaEn (3,0)
-- "en el eje de abscisas"
-- >>> estaEn (0,5)
-- "en el eje de ordenadas"
-- >>>  estaEn (4,4)
-- "en la bisectriz"
-- >>> estaEn (1,2)
--- "en cualquier sitio"

estaEn :: (Int, Int) -> String
estaEn = undefined
