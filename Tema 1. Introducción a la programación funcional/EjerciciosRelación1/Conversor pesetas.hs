{-
 - Ejercicios Relación 1.
 - Estructura de Datos. 
 - Doble Grado Matemáticas + Ingeniería Informática.
 - Ángel Manuel Soria Gil
-}

import Test.QuickCheck
------------------------------------------------------------------------------------------------------
-- Ejercicio 4. Conversor Pesetas
------------------------------------------------------------------------------------------------------
-- constantes necesarias
unEuro :: Double
unEuro = 166.386

-- función que pasa de pesetas a Euros
pesetasAEuros :: Double -> Double
pesetasAEuros x = x/unEuro

-- función que pasa de euros a pesetas
eurosAPesetas :: Double -> Double
eurosAPesetas x = x*unEuro

-- propiedad que debería cumplirse
p1_pesetas x = pesetasAEuros (eurosAPesetas x) == x

--Pregunta: Tras comprobar la propiedad con el quickCheck falla. ¿Por qué?
{- 
- La propiedad en quickCheck falla puesto que estamos trabajando con double, que no son más que aproximaciones
- puesto que no se utilizan fracciones, si no representaciones decimales que no son exactas, por tanto, la propiedad
- puede fallar por los decimales al realizar cálculos.
-}

-- operador de aproximación
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

-- redefinimos la propiedad de pesetas con este operador
p2_pesetas x = pesetasAEuros (eurosAPesetas x) ~= x
