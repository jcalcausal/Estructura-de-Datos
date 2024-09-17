-------------------------------------------------------------------
-- Estructuras de Datos
-- Grados en Ingeniería Informática, del Software y de Computadores
-- Tema 2. Características de la Programación Funcional
-- Pablo López
-------------------------------------------------------------------

module Tema2TiposAlgebraicos where

{-
   Contenido:

    17. Tipos algebraicos
-}

{-
   Además de trabajar con los tipos predefinidos (tuplas, listas,...),
   en Haskell un programador puede definir sus propios tipos.

   En Java la herramienta fundamental para definir nuevos tipos es la
   clase:

              class NuevaClase {
                      ...
              }

   En Haskell usaremos tipos algebraicos, que declararemos con la
   palabra reservada `data`:

                            declaración de los valores del tipo
                                 /
              data NuevoTipo = ...
                       \
               nombre del nuevo tipo
              (empieza en mayúscula)

   Según la forma de la declaración de los valores del nuevo tipo,
   clasificaremos los tipos algebraicos en:

             - enumerado
             - unión
             - producto
             - general
             - parametrizados
-}

{-
   * Tipo enumerado

   Los tipos algebraicos más simples son los enumerados. La declaración
   simplemente enumera los posibles valores del tipo:

                           constructor de datos
                          (empieza en mayúscula)
                                  /
          data TipoEnumerado = Valor_1 | Valor_2 | ... | Valor_n

   A los valores se les llama constructores de datos, porque sirven
   para construir un valor del nuevo tipo.
-}

--             Constructor de datos (empieza en mayúscula)
--                  /
data Direction = North | South | East | West
                deriving (Show, Eq, Ord)
--                   \
--                cláusula deriving

{-
   Los constructores de datos pueden usarse para construir valores del
   nuevo tipo:
-}

trayectoria :: [Direction]
trayectoria = [North, North, West, South, West, West]

{-
   También podemos usar los constructores de datos como patrones para
   definir funciones por casos:
-}

-- |
-- >>> giraDerecha South
-- West
giraDerecha :: Direction -> Direction
giraDerecha North = East
giraDerecha East  = South
giraDerecha South = West
giraDerecha West  = North

-- |
-- >>> enVertical North
-- True
-- >>> enVertical East
-- False
enVertical :: Direction -> Bool
enVertical North = True
enVertical South = True
enVertical _     = False

{-
   La declaración de un tipo algebraico puede incluir una cláusula
   `deriving`:

          data TipoEnumerado = Valor_1 | Valor_2 | ... | Valor_n
                               deriving (Clase_1, Clase_2,...)
                                           /
                                   clase de tipo

   Haskell deriva automáticamente la instancia correspondiente para
   cada clase de tipo mencionada; es decir, genera el código de las
   funciones de la clase de tipo (es como si Java completara
   automáticamente la implementación de una interfaz).

   En general derivaremos instancias para las clases:

          - `Show`: para imprimir los valores en pantalla
          - `Eq`: para comparar por igualdad
          - `Ord`: para establecer un orden total

   Si no incluimos una cláusula `deriving`, tendremos que definir
   manualmente la instancia de la clase o nuestro tipo no soportará
   las operaciones de esa clase.
-}

{-
   * Tipo unión

   Un tipo unión declara varios constructores de datos, cada
   constructor de datos va acompañado de un único argumento:

                                    tipo del argumento
                                          /
         data TipoUnion = Constructor_1 Arg_1 | Constructor_2 Arg_2 | ...
-}

--           Constructor Argumento
--               \        /
data Degrees = Celsius Double
             | Fahrenheit Double
             deriving (Show)

{-
   Para construir un valor de un tipo enumerado debemos facilitar al
   constructor de datos el valor del argumento:
-}

roomTemp :: Degrees
roomTemp = Celsius 22.5
--            /     \
--     constructor argumento

roomTemp' :: Degrees
roomTemp' = Fahrenheit 75.1

{-
   El constructor de datos se puede usar como patrón al definir
   ecuaciones por casos. Esto permite distinguir los casos y, además,
   acceder al valor del argumento:
-}

-- |
-- frozen (Celsius 0)
-- True
-- >>> frozen (Fahrenheit 60)
-- False
frozen :: Degrees -> Bool
frozen (Celsius c)    = c <= 0
frozen (Fahrenheit f) = f <= 32

-- |
-- >>> toCelsius (Fahrenheit 60)
-- Celsius 15.555555555555555
toCelsius :: Degrees -> Degrees
toCelsius (Celsius c)    = Celsius c
toCelsius (Fahrenheit f) = Celsius ((f - 32) / 1.8)

-- |
-- >>>toFahrenheit (Celsius 0)
-- Fahrenheit 32.0
toFahrenheit :: Degrees -> Degrees
toFahrenheit (Celsius c)    = Fahrenheit (c * 1.8 + 32)
toFahrenheit (Fahrenheit f) = Fahrenheit f

{-
   Realmente un constructor de datos define una función que toma un
   argumento del tipo apropiado y devuelve (construye) un valor del
   nuevo tipo. Por ejemplo, el constructor `Celsius` es una función:

         Celsius :: Double -> Degrees
-}

-- |
-- >>> listToCelsius
-- [Celsius 16.7,Celsius 24.1,Celsius 31.8]
listToCelsius :: [Degrees]
listToCelsius = map Celsius [16.7, 24.1, 31.8]

{-
   * Tipo Producto

   Un tipo producto declara un solo constructor de datos que puede recibir varios
   argumentos:

                          constructor único
                                   /
          data TipoProducto = Constructor Arg_1 Arg_2 .. Arg_n
                                                         /
                                                 varios argumentos

   Se le llama tipo producto porque es un producto cartesiano de los
   tipos de los argumentos.
-}

--        Constructor    Argumentos
--             \           /
data Persona = P String String Int deriving (Show)

{-
   En la declaración de tipo anterior no queda muy claro cuál es el
   papel de cada uno de los argumentos de tipo `String`.

   Podemos hacer más legible nuestra declaración mediante sinónimos
   de tipo:

               type NuevoNombre = TipoExistente

   La diferencia entre `data` y `type` es que el primero introduce un
   nuevo tipo, mientras que el segundo introduce un nombre alternativo
   para un tipo existente.
-}

-- sinónimos de tipos
type Name = String
type Surname = String
type Age = Int

--        Constructor    Argumentos
--             \           /
data Person = Pers Name Surname Age deriving (Show)

{-
   Para construir un valor de un tipo producto debemos facilitar a su
   constructor los valores de todos sus argumentos:
-}

john :: Person
john = Pers "John" "Smith" 35

{-
   Al definir funciones sobre un tipo producto podemos utilizar su constructor
   como patrón:
-}

-- |
-- >>> name john
-- "John"
name :: Person -> Name
name (Pers nm _ _) = nm

-- |
-- >>> surname john
-- "Smith"
surname :: Person -> Surname
surname (Pers _ snm _) = snm

-- |
-- >>> age john
-- 35
age :: Person -> Age
age (Pers _ _ ag) = ag

-- |
-- >>> initials john
-- ('J','S')
initials :: Person -> (Char, Char)
initials (Pers (n:_) (s:_) _) = (n, s)

{-
   * Tipo general

   Un tipo algebraico general es un tipo que combina varios productos:

        data TipoGeneral = Constructor_1 Arg_1 .. Arg_n
                         | ...
                         | Constructor_m Arg_1 .. Arg_k

   Es una suma de productos, por eso a estos tipos se les llama
   algebraicos.
-}

type Modulo = Double
type Argumento = Double

data Complejo = Cartesianas Double Double
              | Polares Modulo Argumento
              deriving (Show)

unComplejo :: Complejo
unComplejo = Cartesianas 2 5

otroComplejo :: Complejo
otroComplejo = Polares 5 (pi/4)

modulo :: Complejo -> Double
modulo (Cartesianas a b) = sqrt (a^2 + b^2)
modulo (Polares r _)     = r

aCartesianas :: Complejo -> Complejo
aCartesianas (Polares r theta) = Cartesianas (r * cos theta) (r * sin theta)
aCartesianas c                 = c

{-
   * Tipo parametrizado

   Un tipo parametrizado toma otros tipos como parámetro, como las clases
   genéricas de Java (ArrayList<T>):

                           tipo parámetro
                                  /
          data TipoParametrizado a = Constructor a
                                                /
                                    argumento de un tipo a

   Al igual que en Java (Map<Integer, String>), podemos tomar varios
   tipos como parámetro:

          data TipoParametrizado a b = Constructor a b
-}

{-
   * El tipo Maybe

   Haskell define un tipo parametrizado `Maybe` que es muy útil para representar
   resultados opcionales:

               data Maybe a = Nothing
                            | Just a

   Si una función es parcial (no está definida para todas sus
   entradas) la podemos hacer total haciendo que devuelva un resultado
   de tipo `Maybe`.

   Puedes pensar que el tipo `Maybe a` es como una caja que puede:

     - estar vacía (Nothing)
     - contener un solo dato de tipo a (Just a)

-}

-- |
-- >>> recíproco 2
-- Just 0.5
-- >>> recíproco 0
-- Nothing
recíproco :: Double -> Maybe Double
recíproco x = if x /= 0
              then Just (1 / x)
              else Nothing

cogerDato :: Maybe Double -> Double
cogerDato Nothing  = error "caja vacía"
cogerDato (Just x) = x

cartera :: [(String, Double)]
cartera = [("intel", 700), ("google", 1000), ("nvidia", 400)]

-- |
-- >>> cotización "google" cartera
-- Just 1000.0
-- >>> cotización "apple" cartera
-- Nothing
cotización :: String -> [(String, Double)] -> Maybe Double
cotización empresa cartera = buscar empresa cartera

buscar :: Eq a => a -> [(a, b)] -> Maybe b -- predefinida como lookup
buscar _ [] = Nothing
buscar x ((k, v) : ps)
  | x == k = Just v
  | otherwise = buscar x ps
