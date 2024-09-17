------------------------------------------------------------------
-- Estructuras de Datos
-- Grado en Ingeniería Informática, del Software y de Computadores
-- Tema 1. Introducción a la Programación Funcional
-- Pablo López
------------------------------------------------------------------

module QuickCheckBasico where

-- Uso básico de QuickCheck

-- Para usar QuickCheck es necesario importar el módulo:

import           Test.QuickCheck

-- definición de cuadrado

cuadrado :: Integer -> Integer
cuadrado x = x * x

-- Estrategias de testing:

-- 1) test efímero y manual

-- directamente en el intérprete

-- 2) test reproducible y manual (como en POO)

-- programa de prueba

main :: IO()
main = print (cuadrado 5)

-- 3) test reproducible y automatizado (test unitario)

test_cuadrado :: Bool
test_cuadrado = cuadrado 5 == 25  &&  -- entrada y salida conocidas
                cuadrado 7 == 49

-- 4) test de propiedades (con QuickCheck)

-- comprobamos que salidas desconocidas verifican ciertas propiedades

{-

Las propiedades QuickCheck tienen el aspecto:

    prop_nombre :: t1 -> t2 -> ... -> tn -> Bool
    prop_nombre arg1 arg2 ... argn = expr_booleana

es decir, una propiedad es una función que devuelve Bool

-}

-- propiedades del cuadrado

-- el cuadrado de un número no es negativo

prop_cuadrado_no_negativo :: Integer -> Bool
--                       candidatos    propiedad
prop_cuadrado_no_negativo     x   =  cuadrado x >= 0

-- para probar una propiedad, hay que invocar la función quickCheck
-- con el nombre de la propiedad SIN ARGUMENTOS; por ejemplo:

-- Main> quickCheck prop_cuadrado_no_negativo

prop_cuadrado_binomio :: Integer -> Integer -> Bool
prop_cuadrado_binomio x y  =
    cuadrado (x + y) == cuadrado x + cuadrado y + 2*x*y

-- propiedades del valor absoluto

prop_desigTriangular :: Integer -> Integer -> Bool
prop_desigTriangular x y =
    abs (x + y) <= abs x + abs y

-- la siguiente expresión calcula el máximo de dos valores:

máximo :: Integer -> Integer -> Integer
máximo x y = (x + y + abs (x - y)) `div` 2

-- comprobamos que es correcta comparándola con otra implementación:

prop_máximo_OK :: Integer -> Integer -> Bool
prop_máximo_OK x y =
  máximo x y == max x y

-- ¿por qué no funciona esta propiedad?
prop_modulo1_WRONG :: Integer -> Integer -> Bool
prop_modulo1_WRONG x y =
  abs (x `mod` y) < abs y

{-

Algunas propiedades QuickCheck tienen precondiciones:

prop_nombre :: t1 -> t2 -> ... -> tn -> Property
prop_nombre arg1 arg2 ... argn =
       precondición ==> expr_booleana

la precondición es una expresión booleana que filtra los
candidatos propuestos por QuickCheck.

Las propiedades QuickCheck con precondición tienen tipo Property, no Bool.

-}

prop_modulo1 :: Integer -> Integer -> Property
--           candidatos     filtro            propiedad
prop_modulo1     x y     =  y /= 0  ==> abs (x `mod` y) < abs y

prop_modulo2 :: Integer -> Integer -> Property
--           candidatos       filtro          propiedad
prop_modulo2     x y     = x > 0 && y > x ==> x == x `mod` y
