-------------------------------------------------------------------------------
-- Estructuras de Datos. 2º ETSI Informática. UMA
-- Práctica 1
-- Doble Grado Matemáticas + Ingeniería Informática
-- Alumno: Ángel Manuel Soria Gil
-------------------------------------------------------------------------------

import Test.QuickCheck

---------------------------------------------------------------------------------------------------------
-- Ejercicio 1. Números primos
---------------------------------------------------------------------------------------------------------
esPrimo :: (Integral a) => a -> Bool
esPrimo x = esPrimoAux x 2

esPrimoAux :: (Integral a) => a -> a -> Bool
esPrimoAux x y 
   | x <= 0 = error "Argumento no válido. No se habla de primos para enteros no positivos."
   | x == 1 = False
   | x `mod` y == 0 && y<x = False
   | x `mod` y /= 0 && y<x = esPrimoAux x (y+1)
   | otherwise = True

-------------------------------------------------------------------------------------------------------------
-- Ejercicio 2. Cociente y Resto 
-------------------------------------------------------------------------------------------------------------
cocienteYResto :: (Integral a) => a -> a -> (a, a)
cocienteYResto x y = cocienteYRestoAux x y 0

cocienteYRestoAux :: (Integral a) => a -> a -> a -> (a, a)
cocienteYRestoAux x y c 
   | x < 0 || y<=0 = error "Argumentos no válidos. Ambos deben ser enteros positivos."
   | x >= y       = cocienteYRestoAux (x-y) y (c+1)
   | otherwise    = (c, x)

p1_CocienteYResto x y =  x > 0 && y > 0 ==> (x `div` y == c) && (x `mod` y == r)
      where (c, r) = cocienteYResto x y

--------------------------------------------------------------------------------------------------------------
-- Ejercicio 3. Libre de Cuadrados.
--------------------------------------------------------------------------------------------------------------
libreDeCuadrados :: (Integral a) => a -> Bool
libreDeCuadrados x = libreDeCuadradosAux x 2

libreDeCuadradosAux :: (Integral a) => a -> a -> Bool
libreDeCuadradosAux x y 
   | x <= 0 = error "Argumento no válido. No se habla de primos para enteros no positivos."
   | x == 1 = False
   | x `mod` (y^2) == 0 && (y^2) < x = False
   | x `mod` (y^2) /= 0 && (y^2) < x = libreDeCuadradosAux x (y+1)
   | x == y^2 = False
   | otherwise = True

------------------------------------------------------------------------------------------------------------
-- Ejercicio 4. Raiz Entera
------------------------------------------------------------------------------------------------------------
raizEntera :: Integer -> Integer
raizEntera x = raizEnteraAux x 0

raizEnteraAux :: Integer -> Integer -> Integer
raizEnteraAux x y 
   | x < 0 = error "Argumento no válido. La raiz cuadrada se define para enteros no negativos."
   | x <= 1 = x
   | x > 1 && y^2 <=x = raizEnteraAux x (y+1)
   | otherwise = y-1

prop1_raizEntera n = n >= 0 ==> truncate (sqrt (fromIntegral n)) == raizEntera n

raizEnteraRapida :: Integer -> Integer
raizEnteraRapida n = raizEnteraRapidaAux n 0 n

raizEnteraRapidaAux :: Integer -> Integer -> Integer -> Integer
raizEnteraRapidaAux n o f
   | n < 0 = error "Argumento no válido. La raiz cuadrada se define para enteros no negativos."
   | n <= 1 = n
   | m^2 <= n && (m+1)^2 > n = m
   | m^2 > n = raizEnteraRapidaAux n 0 m
   | m^2 < n = raizEnteraRapidaAux n m n
   where 
      m = n `div` 2

-- propiedad : raizEntera y RaizEnteraRápida deben coincidir
prop2_raizEntera n = n>=0 ==> raizEntera n == raizEnteraRapida n 

--------------------------------------------------------------------------------------------------------
-- Ejercicio 5. Harshad
--------------------------------------------------------------------------------------------------------
sumaDigitos :: Integer -> Integer 
sumaDigitos x
   | x < 0 = error "Argumento no válido. Buscamos un número entero no negativo"
   | x `div` 10 == 0 =  x
   | otherwise = x `mod` 10 + sumaDigitos (x `div` 10)

-- dice si un número es de harshad, es decir, si es divisible por la suma de sus dígitos
harshad :: Integer -> Bool
harshad x 
   | x == 0 = error "Argumento no válido. Buscamos números enteros positivos"
   | otherwise = x `mod` (sumaDigitos x) == 0

-- dice si es dos veces harshad, es decir, él es harshad y el cociente x/sumadigitos(x) también lo es
harshadMultiple :: Integer -> Bool 
harshadMultiple x = harshad x && harshad (x `div` (sumaDigitos x))

-- dice las veces que un número es múltiple de harshad.
vecesHarshad :: Integer -> Integer
vecesHarshad x 
   | x == 1    = 1
   | harshad x = 1 + vecesHarshad (x `div` (sumaDigitos x))
   | otherwise = 0 

-- propiedad de Bloem Harshad.
prop1_Bloem_Harshad n = n>=0 ==> vecesHarshad (1008*(10^n)) == n+2 

----------------------------------------------------------------------------------------------------------
-- Ejercicio 6. Ceros de.
----------------------------------------------------------------------------------------------------------
cerosDe :: Integer -> Integer
cerosDe x 
   | x == 0          = 1
   | x `mod` 10 == 0 = 1 + cerosDe (x `div` 10)
   | otherwise = 0

-- propiedad de cerosDe
prop1_cerosDe n m = m>0 && m<1000 ==> cerosDe z == m
   where z = n * (10^m) 
-- esto falla

-----------------------------------------------------------------------------------------------------------
-- Ejercicio 7. Fibonacci.
-----------------------------------------------------------------------------------------------------------
fib :: Integer -> Integer
fib n 
   | n < 0  = error "los índices de una sucesión son naturales ó 0"
   | n == 0 = 0
   | n == 1 = 1
   | n > 1  = fib (n-1) + fib (n-2)

llamadasFib :: Integer -> Integer
llamadasFib n 
   | n < 0 = error "no se puede llamar a fib para n<0"
   | n == 0 = 1
   | n == 1 = 1
   | n > 1  = 1 + llamadasFib (n-1) + llamadasFib (n-2)

fib' :: Integer -> Integer 
fib' n = fibAc n 0 1
   where fibAc n a b | n == 0 = a 
                     | n > 0  = fibAc (n-1) b  (a+b)

binet :: Integer -> Integer 
binet n = round(((fi^n) + (- (1 - fi)^n))/r5)
   where 
      fi = ((1 + r5) / 2) :: Double 
      r5 = sqrt 5


