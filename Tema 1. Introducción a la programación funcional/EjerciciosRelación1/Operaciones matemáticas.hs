{-
 - Ejercicios Relación 1.
 - Estructura de Datos. 
 - Doble Grado Matemáticas + Ingeniería Informática.
 - Ángel Manuel Soria Gil
-}

import Test.QuickCheck
------------------------------------------------------------------------------------------------------
-- Ejercicio 5. Raices de polinomios de segundo grado
------------------------------------------------------------------------------------------------------
-- operadores necesarios
-- operador de aproximación
infix 4 ~=
(~=) :: Double -> Double -> Bool
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

-- función que devuelve las raices del polinomio ax^2+bx+c    
raices :: Double -> Double -> Double -> (Double, Double)
raices a b c 
    | disc < 0 = error "no existen raices reales"
    | otherwise = (r1, r2) 
    where 
        disc = b^2 - 4*a*c
        r1 = ((-b) + (sqrt disc)) / (2*a)
        r2 = ((-b) - (sqrt disc)) / (2*a)

-- propiedades de la función raices
p1_raices a b c = esRaiz r1 && esRaiz r2
    where 
        (r1, r2) = raices a b c
        esRaiz r = a*r^2 + b*r + c ~= 0

{- 
- Pregunta: Esta función falla con el quickCheck, ¿por qué?
- Creo que se falla porque no se comprueba el discriminante positivo
- Otra condición necesaria sería que a sea distinto de 0 pues de ser 0 no se puede calcular con esa fórmula
-}

p2_raices a b c = a /= 0 && disc >= 0 ==> esRaiz r1 && esRaiz r2
    where 
        disc = b^2 - 4*a*c
        (r1, r2) = raices a b c
        esRaiz r = a*r^2 + b*r + c ~= 0


-------------------------------------------------------------------------------------------------------------
-- Ejercicio 6. Múltiplos
-------------------------------------------------------------------------------------------------------------
-- función para determinar si un valor es múltiplo de otro
esMultiplo :: (Integral a) => a -> a -> Bool
esMultiplo x y = x `mod` y == 0

------------------------------------------------------------------------------------------------------------
-- Ejercicio 7. Implicación lógica.
------------------------------------------------------------------------------------------------------------
infixl 1 ==>>
(==>>) :: Bool -> Bool -> Bool
False ==>> _ = True
True ==>> x  = x 

-----------------------------------------------------------------------------------------------------------
-- Ejercicio 8. Año Bisiesto.
-----------------------------------------------------------------------------------------------------------
esBisiesto :: Integer -> Bool
esBisiesto x 
    | (esMultiplo x 4) && (esMultiplo x 100) = (esMultiplo x 400)
    | (esMultiplo x 4) = True
    | otherwise = False

-----------------------------------------------------------------------------------------------------------
-- Ejercicio 9. Función Potencia.
-----------------------------------------------------------------------------------------------------------
-- función potencia con b^n = b*b^n-1
potencia :: Integer -> Integer -> Integer
potencia b n 
    | n < 0 = error "el exponente debe ser natural o 0"
    | n == 0 = 1
    | otherwise = b * (potencia b (n-1))

-- función potencia con distinción de n par e impar
potencia' :: Integer -> Integer -> Integer
potencia' b n 
    | n < 0 = error "el exponente debe ser natural o 0"
    | n == 0 = 1
    | n == 1 = b
    | (n `mod` 2 == 0) =  (potencia' b (n `div` 2))^2
    | otherwise = b * (potencia' b ((n-1) `div` 2))^2

-- propiedad de potencia y potencia'
p1_pot b n = n>=0 ==> (potencia b n == b^n) && (potencia' b n == b^n)

---------------------------------------------------------------------------------------------------------
-- Ejercicio 10. Factorial
---------------------------------------------------------------------------------------------------------
factorial :: Integer -> Integer
factorial x
    | x < 0     = error "el factorial no está definido para enteros negativos" 
    | x == 0    = 1
    | x >=1     = x * factorial (x-1)

---------------------------------------------------------------------------------------------------------
-- Ejercicio 11. División Entera.
---------------------------------------------------------------------------------------------------------
-- función que devuelve si x divide a y
divideA :: (Integral a) => a -> a -> Bool
divideA x y 
    | x == 0    = error "el denominador no puede ser 0"
    | otherwise = (y `mod` x) == 0

-- propiedades divideA
p1_divideA x y = x/=0 && x `divideA` y ==> (y `div` x)*x == y

p2_divideA x y z = (x/=0) && (x `divideA` y) && (x `divideA` z) ==> x `divideA` (y+z)

----------------------------------------------------------------------------------------------------------
-- Ejercicio 12. Mediana de 5 valores
----------------------------------------------------------------------------------------------------------
mediana :: (Ord a) => (a, a, a, a, a) -> a
mediana (p, q, r, s, t)
    | p > r     = mediana (r, q, p, s, t)
    | q > r     = mediana (p, r, q, s, t)
    | s < r     = mediana (p, q, s, r, t)
    | t < r     = mediana (p, q, t, s, r)
    | otherwise = r
