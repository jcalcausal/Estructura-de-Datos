-------------------------------------------------------------------------------- 
-- Estructuras de Datos. 2º Curso. ETSI Informática. UMA -
-- Titulación: Doble Grado en Matemáticas e Ingeniería Informática.
-- Alumno: SORIA GIL, ÁNGEL MANUEL
-- Fecha de entrega: 27 | 09 | 2022
-- Relación de Ejercicios 1. Ejercicios resueltos: .......... --
-------------------------------------------------------------------------------
import Test.QuickCheck


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 1 -- esTerna
---------------------------------------------------------------------------------------------------------------------
--1(a)
esTerna :: Integer -> Integer -> Integer -> Bool
esTerna x y z = x^2 + y^2 == z^2
--1(b)
terna :: Integer -> Integer -> (Integer, Integer, Integer)
terna x y | y > 0 && x > y = (x^2-y^2, 2*x*y, x^2+y^2)
 --1(c, d)           
p_terna :: Integer -> Integer -> Bool
p_terna x y | (x>0 && y>0 && x>y) = esTerna l1 l2 h           
                where 
                    (l1, l2, h) = terna x y


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 2 -- intercambia
---------------------------------------------------------------------------------------------------------------------
intercambia :: (a, b) -> (b, a)
intercambia (x, y) = (y, x)


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 3 -- ordena
---------------------------------------------------------------------------------------------------------------------
--3(a)
ordena2 :: Ord a => (a,a) -> (a,a)
ordena2 (x, y) | x < y = (x, y)
               | y >= x = (y, x)

p1_ordena2 :: Ord a => a -> a -> Bool
p1_ordena2 x y = enOrden (ordena2 (x,y))
    where enOrden (x,y) = x<=y

p2_ordena2 :: Ord a => a -> a -> Bool 
p2_ordena2 x y = mismosElementos (x,y) (ordena2(x,y))
    where mismosElementos (x,y) (z,v) = (x==z && y==v)||(x==v && z==y)

--3(b)
ordena3 :: Ord a => (a,a,a) -> (a,a,a)
ordena3 (x,y,z) = (s,v,t)
    where
        s = min (min x y) z
        t = max (max x y) z 
        v | s == x = min y z 
          | s == y = min x z 
          | s == z = min x y   

p1_ordena3 :: Ord a => a -> a -> a -> Bool
p1_ordena3 x y z = enOrden3 (ordena3(x,y,z))
    where enOrden3(x,y,z) = x<=y && y<=z

p2_ordena3 :: Ord a => a -> a -> a -> Bool
p2_ordena3 x y z = mismosElementos3 (x,y,z) (ordena3(x,y,z))
    where 
        mismosElementos3 (x,y,z) (s,v,t) = (x,y,z)==(s,v,t)||(x,y,z)==(s,t,v)||(x,y,z)==(v,s,t)||(x,y,z)==(v,t,s)||(x,y,z)==(t,v,s)||(x,y,z)==(t,s,v)


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 4. -- max
---------------------------------------------------------------------------------------------------------------------
--4(a)
max2 :: Ord a => a -> a -> a
max2 x y | x>=y = x
         | y>=x = y
         | otherwise = max x y

--4(b)
p1_max2 :: Ord a => a -> a -> Bool
p1_max2 x y = (max2 x y) == x || (max2 x y) == y 

p2_max2 :: Ord a => a -> a -> Bool
p2_max2 x y = max2 x y >= x && max2 x y >= y

p3_max2 :: Ord a => a -> a -> Bool
p3_max2 x y = if x>=y then max2 x y == x else False

p4_max2 :: Ord a => a -> a -> Bool
p4_max2 x y = if y>=x then max2 x y == y else False


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 5 -- entre 
---------------------------------------------------------------------------------------------------------------------
entre :: Ord a => a -> (a,a) -> Bool
entre s (x,y) | x>y = undefined
              | x<=s && s<=y = True
              | otherwise = False


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 6 -- iguales
---------------------------------------------------------------------------------------------------------------------
iguales3 :: Eq a => (a,a,a) -> Bool
iguales3 (x,y,z) = (x == y) && (y == z) 


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 7 -- descomponer
---------------------------------------------------------------------------------------------------------------------
--7(a)
type TotalSegundos = Integer
type Horas = Integer
type Minutos = Integer
type Segundos = Integer
descomponer :: TotalSegundos -> (Horas, Minutos, Segundos)
descomponer x = (h, m, s)
    where 
        s = mod x 60
        m = mod (div x 60) 60
        h = div x 3600

--7(b)
p_descomponer :: TotalSegundos -> Bool
p_descomponer x = x>=0 && h*3600 + m*60 + s == x && entre m (0,59) && entre s (0,59)
    where (h,m,s) = descomponer x
        

---------------------------------------------------------------------------------------------------------------------
--Ejercicio 8 -- pesetasAEuros y viceversa
---------------------------------------------------------------------------------------------------------------------
--8(a)
unEuro :: Double
unEuro = 166.386

pesetasAEuros :: Double -> Double
pesetasAEuros x =  x/unEuro

--8(b)
eurosAPesetas :: Double -> Double 
eurosAPesetas x = x*unEuro

--8(c)
p_inversas:: Double -> Bool
p_inversas x = x == pesetasAEuros (eurosAPesetas x)
--El quickCheck falla porque al ser Doubles y trabajar con punto flotante en la división se comete un error al tomar decimales.


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 9 -- aprximación
---------------------------------------------------------------------------------------------------------------------
(~=) :: Double -> Double -> Bool 
x ~= y = abs (x-y) < epsilon
    where epsilon = 1/1000

p_inversas2 :: Double -> Bool
p_inversas2 x = x ~= pesetasAEuros (eurosAPesetas x)


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 10 -- raices
---------------------------------------------------------------------------------------------------------------------
raices :: Double -> Double -> Double -> (Double, Double)
raices a b c | b^2 - 4*a*c < 0 = error "RaicesNoReales"
             | otherwise = (r1, r2)
                    where r1 = (-b + sqrt discriminante)/(2*a)
                          r2 = (-b - sqrt discriminante)/(2*a)
                          discriminante = b^2 - 4*a*c

--p1_raices :: Double -> Double -> Double -> Bool
--p1_raices a b c = esRaíz r1 && esRaíz r2 
--    where
--        (r1,r2) = raíces a b c
--        esRaíz r = a*r^2 + b*r + c ~= 0

--p2_raices :: Double -> Double -> Double -> Bool
--p2_raices a b c = ??????? && ?????? ==> esRaíz r1 && esRaíz r2 
  --  where
    --    (r1,r2) = raíces a b c
    --    esRaíz r = a*r^2 + b*r + c ~= 0


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 11
---------------------------------------------------------------------------------------------------------------------
esMultiplo :: Integral a => a -> a -> Bool
esMultiplo x y = mod x y == 0


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 12
---------------------------------------------------------------------------------------------------------------------
--(==>>) :: Bool -> Bool -> Bool


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 13
---------------------------------------------------------------------------------------------------------------------
esBisiesto :: Integer -> Bool
esBisiesto x = (mod x 4) == 0 && (if (mod x 100) == 0 then (mod x 400) == 0 else True)


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 14
---------------------------------------------------------------------------------------------------------------------
-- 14.a
potencia :: Integer -> Integer -> Integer
potencia b n | n < 0   = error "ExponenteNoValido"
             | n ==  0 = 1
             | n > 0   = b * potencia b (n-1)  

-- 14.b
potencia' :: Integer -> Integer -> Integer
potencia' b n   | n < 0   = error "ExponenteNoValido"
                | n == 0 = 1
                | mod n 2 == 0 = potencia' (potencia' b (div n 2)) 2
                | otherwise = b * potencia' (potencia' b (div (n-1) 2)) 2

-- 14.c
p_pot b n =
   potencia b m == sol && potencia' b m == sol
   where sol = b^m
         m = abs n

-- 14.d


---------------------------------------------------------------------------------------------------------------------
--Ejercicio 15 -- factorial
---------------------------------------------------------------------------------------------------------------------
factorial :: Integer -> Integer
factorial x | x==0 = 1
            | otherwise = x*factorial (x-1)



---------------------------------------------------------------------------------------------------------------------
-- Ejercicio 16 -- divide a 
---------------------------------------------------------------------------------------------------------------------
-- 16 a.
divideA :: Integer -> Integer -> Bool
divideA x y | y == 0 = error "DivisiónPor0"
            | otherwise = if (mod x y == 0) then True else False

--16 b.
p1_divideA x y = y/=0 && y `divideA` x ==> div x y * y == x

--16 c.
p2_divideA x y z = divideA x z && divideA y z ==> divideA (x+y) z 
    


--------------------------------------------------------------------------------------------------------------------
-- Ejercicio 17 -- mediana5
--------------------------------------------------------------------------------------------------------------------
mediana5 :: Ord a => (a, a, a, a, a) -> a 
mediana5 (x1, x2, x3, x4, x5) 
        | x1 > x3 = mediana5 (x3, x2, x1, x4, x5)
        | x2 > x3 = mediana5 (x1, x3, x2, x4, x5)
        | x4 < x3 = mediana5 (x1, x2, x4, x3, x5)  
        | x5 < x3 = mediana5 (x1, x2, x5, x4, x3)
        | otherwise = x3
