module Kikis where
import PdePreludat

--1)
data Paquete = UnPaquete{
    nombreDestinatario :: String,
    peso :: Number,
    delicadeza :: Number,
    esUrgente :: Bool
} deriving (Show, Eq)

peluche :: Paquete
peluche = UnPaquete{
    nombreDestinatario = "Ket",
    peso = 1,
    delicadeza = 3,
    esUrgente = True
}

pintura :: Paquete
pintura = UnPaquete{
    nombreDestinatario = "Ursula",
    peso = 4,
    delicadeza = 9,
    esUrgente = False
}

torta :: Paquete
torta = UnPaquete{
    nombreDestinatario = "Osono",
    peso = 2,
    delicadeza = 6,
    esUrgente = False
}

--2) 
type Vuelo = [Paquete]
caca :: Vuelo
caca = [peluche, pintura, torta]

protegible :: Number -> Paquete -> Bool
protegible magia paquete =  magia > delicadeza paquete

paquetesQuePuedeLLevar  :: Number -> Vuelo -> [Paquete]
paquetesQuePuedeLLevar  magia vuelo = filter (protegible magia) vuelo

--3)
data Clima = Llueve | Nevado | Soleado deriving(Show, Eq) 
recargo :: Number -> Clima -> Paquete -> Number
recargo magia palabra paquete 
    | palabra == Llueve = 30 + (peso paquete) * magia
    | palabra == Nevado = 50 + (peso paquete) * magia
    | palabra == Soleado = (peso paquete) * magia

--4) 
esAfortunado :: Vuelo -> Bool
esAfortunado = all traeSuerte

traeSuerte :: Paquete -> Bool
traeSuerte paquete = even (length (nombreDestinatario paquete))

traeSuerte2 :: Paquete -> Bool
traeSuerte2 = even . length . nombreDestinatario

--5)
esAccesible :: Paquete -> Bool
esAccesible paquete = 2 > peso paquete

esAccesible' :: Paquete -> Bool
esAccesible' paquete = (recargo 10 Llueve paquete) < 50

--6)
esDificilDeManiobrar :: Vuelo -> [Paquete]
esDificilDeManiobrar vuelo = filter condicionManiobrar vuelo

condicionManiobrar :: Paquete -> Bool
condicionManiobrar paquete = peso paquete > mitadDelicadeza paquete

mitadDelicadeza :: Paquete -> Number
mitadDelicadeza paquete = delicadeza paquete / 2

--forma con Landa (λ):
esDificilDeManiobrar2 :: Vuelo -> [Paquete]
esDificilDeManiobrar2 = filter (\paquete -> peso paquete > mitadDelicadeza paquete)

condicionManiobrar2 :: Paquete -> Bool
condicionManiobrar2 = \paquete -> peso paquete > mitadDelicadeza paquete

mitadDelicadeza2 :: Paquete -> Number
mitadDelicadeza2 = \x -> delicadeza x / 2

--7)
type Hechizo =  Paquete -> Paquete

reducirPeso :: Number -> Paquete -> Paquete
reducirPeso x paquete = paquete { peso = peso paquete - x }

alivianar :: Hechizo
alivianar paquete 
    | peso paquete > 2 = reducirPeso 2 paquete
    | otherwise = paquete { peso = 0 }

reducirDelicadeza :: Number -> Paquete -> Paquete
reducirDelicadeza x paquete = paquete { delicadeza = delicadeza paquete - x }

reforzar :: Number -> Hechizo
reforzar x paquete = reducirDelicadeza x paquete

hechizoPaciencia :: Hechizo
hechizoPaciencia paquete 
     | esUrgente paquete == True = paquete {esUrgente = False}
     | otherwise = id paquete


--8)
type Catalogo = [Hechizo]

catalogoKiki :: Catalogo
catalogoKiki = [alivianar, hechizoPaciencia]

catalogoPotente :: Catalogo
catalogoPotente = [alivianar, reforzar 3, reforzar 10]

--9) PUNTO DIFICIL (PARA REPETIR)

simularHechizo :: Paquete -> Catalogo -> [Paquete]
simularHechizo paquete catalogo = map ($ paquete) catalogo

simularHechizo' :: Paquete -> Catalogo -> [Paquete]
simularHechizo' paquete catalogo = map (\hechizo -> hechizo paquete) catalogo



-- PARA ENTENDER LAMBDA Y EL SIGNO $
sumarXATodos :: Number -> [Number] -> [Number]
sumarXATodos valor lista = map (\x -> x + valor) lista
-- => [(\x -> x + 4) 1, (\x -> x + 4) 2, (\x -> x + 4) 3]

sumar :: Number -> Number -> Number
sumar x y = x + y
sumarXATodos' :: Number -> [Number] -> [Number]
sumarXATodos' valor lista = map (sumar $ valor) lista 
-- => [(sumar $ 4) 1,(sumar $ 4) 2,(sumar $ 4) 3]