APUNTES DE LA CLASE:
identidad :: a -> a
identidad x = x

entre :: Ord a => a -> a -> a -> Bool
entre bajo alto medio = bajo <= medio && medio <= alto

entreSoloNumeros :: Number -> Number -> Number -> Bool
entreSoloNumeros bajo alto medio = bajo <= medio && medio <= alto


-Clases de Tipo:
eq = valorques que puedo igualar (==)
show = te devuelve su valor en string
ord = ordenado / variables que tienen un orden ( <  <=  >  >= )


deriving show = permite que se pueda hacer show para los valores del data
deriving eq = permite que se pueda hacer show para los valores del data
(los data no son mostrables por defecto)


Las funciones se pueden pasar por parametros => Las FUNCIONES son VALORES
Una funcion es de orden superior si recibe como parametro otra funcion
ej: all even[2,4,6,8] : aca la funcion even se pasa por parametro y la funcion all es de orden superior


FUNCIONES IMPORTANTES:
all = chequea si cumple todo segun la segunda funcion que le pases
any = cheque si alguno cumple segun la segunda funcion que le pases
filter = filtra segun la segunda funcion que le pases
show = te devuelve su valor en string (toString)
odd = impar
even = par
map = le aplica la funcion que pusimos a cada elemento de la lista (devuelve lista)

:t all == all :: (a -> Bool) -> [a] -> Bool
:t any == any :: (a -> Bool) -> [a] -> Bool
:t filter == filter :: (a -> Bool) -> [a] -> [a]
:t show ==  show :: Show a => a -> String
:t even / odd == even/odd :: Number -> Bool
:t map == map :: (a -> b) -> [a] -> [b]
