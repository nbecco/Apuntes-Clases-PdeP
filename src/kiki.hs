module Kiki where
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

paqueteQuePuedeLlevar :: Number -> Vuelo -> Vuelo
paqueteQuePuedeLlevar magia vuelo = filter (protegible magia) vuelo


--3) NO ES FUNDAMENTAL POR AHORA (SEGUN EL PROFE


--4) 
esAfortunado :: Vuelo -> Bool
esAfortunado = all traeSuerte

traeSuerte :: Paquete -> Bool
traeSuerte paquete = even (length (nombreDestinatario paquete))

traeSuerte2 :: Paquete -> Bool
traeSuerte2 = even . length . nombreDestinatario


--5) LO SALTEO EL PROFE


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

--7) ???
-- type Hechizo =  Paquete -> Paquete

-- reducirPeso :: Number -> Paquete -> Number
-- reducirPeso x paquete = peso paquete - x

-- alivianar :: Hechizo
-- alivianar paquete = reducirPeso 2 paquete

-- reforzar :: Number -> Hechizo
-- reforzar x = reducirPeso x

-- hechizoPaciencia :: Hechizo
-- hechizoPaciencia paquete 
--     | esUrgente paquete == True = not (esUrgente paquete)
--     | otherwise = paquete


--8) FALTA HACERLO (AUNQUE EL PROFE YA LO HIZO EN CLASE)


--9) FALTA HACERLO (AUNQUE EL PROFE YA LO HIZO EN CLASE)