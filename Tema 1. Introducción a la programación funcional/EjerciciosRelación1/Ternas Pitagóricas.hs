{-
 - Ejercicios Relación 1.
 - Estructura de Datos. 
 - Doble Grado Matemáticas + Ingeniería Informática.
 - Ángel Manuel Soria Gil
-}

import Test.QuickCheck
------------------------------------------------------------------------------------------------------
-- Ejercicio 1. Ternas Pitagóricas.
------------------------------------------------------------------------------------------------------
-- Comprueba si tres valores forman una terna pitagórica.
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x^2 + y^2 == z^2

-- Dados dos valores nos construye una terna pitagórica.
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y = (x^2-y^2, 2*x*y, x^2+y^2)

-- Propiedad ternas
p_ternas x y = x>0 && y>0 && x>y ==> esTerna a b c
    where (a, b, c) = terna x y


