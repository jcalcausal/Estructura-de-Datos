----------------------------------------------------------
-- Ejercicios Extra Tema 1
-- Author: Ángel Manuel Soria Gil
----------------------------------------------------------

--Ejercicio 1. Comprobramos si un entero es primo o no. Tenemos restricciones,
--no se aceptan números negativos ni el cero.
esPrimo :: Integer -> Bool
esPrimo x | x==1 = True
          | x==2 = True
          | x>2 = esPrimoAux x 2 
          | otherwise = error "Argumento no válido" 

esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux x y | y < x = if mod x y == 0 then False else esPrimoAux x (y+1)
               | y>=x  = True 


--Ejercicio 2. Dar una dupla con el cociente y resto de dividir a entre b. Debemos
--tener en cuenta las restricciones pedidas, no usar ni operaciones ya definidas
--para ello ni admitiendo negativos. Ademáss hacer un quickCheck.
cocienteYResto :: (Integer, Integer) -> (Integer, Integer)
cocienteYResto (x,y) = ((cociente(x,y)), (resto(x,y)))

cociente :: (Integer, Integer) -> Integer
cociente (x,y) | x < 0 || y <= 0 = error "Argumentos no válidos"
               |x<y  = 0
               | x==y = 1
               | x>y  = 1 + cociente ((x-y), y)

resto :: (Integer, Integer) -> Integer
resto (x,y) | x < 0 || y <= 0 = error "Argumentos no válidos"
            | x < y  = x
            | x == y = 0
            | x > y  = resto ((x-y), y)

prop_CocienteYResto_OK (x, y) = (cociente(x,y)*y + resto (x,y)) == x


--Ejercicio 3. Devolver si un número natural está libre de cuadradados perfectos,
--es decir, devuelve true si no tiene ningún cuadrado perfecto en su descomposición
--en factores primos y false en caso contrario.
libreDeCuadrados :: Integer -> Bool
libreDeCuadrados x | x<=0 = error "argumento negativo o cero"
                   | x==1 = True
                   | x==2 = True
                   | otherwise = libreDeCuadradosAux x 2

libreDeCuadradosAux :: Integer -> Integer -> Bool
libreDeCuadradosAux x y | x<(y*y) = True
                         | otherwise = if mod x (y*y) == 0 then False else libreDeCuadradosAux x (y+1) 

--Ejercicio 4. Devolver la raíz entera de un número probando uno a uno.
raizEntera :: Integer -> Integer
raizEntera x |x == 1 = 1
             |otherwise =  raizEnteraAuxiliar x 1

raizEnteraAuxiliar :: Integer -> Integer -> Integer
raizEnteraAuxiliar x y | x<=0 = error "argumento negativo o cero"
                       | x>y = if (y*y) > x then y-1 else raizEnteraAuxiliar x (y+1)

--Ejercicio 5. Devolver la raiz entera de un número pero sin probar uno a uno,
--acotar primero donde estará la raiz y buscarla allí.
