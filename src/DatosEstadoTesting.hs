module DatosEstadoTesting where
import PdePreludat

type Persona = (String, Edad, Number)
type Edad = Number

doble :: Number -> Number
doble numero = numero + numero

pepita :: Persona
pepita = ("Pepita", 11, 30)

edad :: Persona -> Edad
edad (_, edad, _) = edad

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad persona = edad persona > 18

valorDeRareza :: [Number] -> Number
valorDeRareza numeros = length numeros * 2


data Jovit = UnJovit{
    nombre :: String,
    estatura :: Number,
    fuerza :: Number,
    esDeLaComarca :: Bool
} deriving Show


bilbo :: Jovit
bilbo = UnJovit{
    nombre = "Bilbo", 
    esDeLaComarca=True,
    estatura=125, 
    fuerza=20
    }

bilbo2 :: Jovit
bilbo2 = UnJovit "Bilbo" 125 20 True


longitudNombre :: Jovit -> Number
longitudNombre jovit = length (nombre jovit)

reputacion :: Jovit -> Number
reputacion jovit = longitudNombre jovit * estatura jovit

diferenciaDeAltura :: Jovit -> Jovit -> Number
diferenciaDeAltura jovit1 jovit2 = abs (estatura jovit1 - estatura jovit2)

-- Version engorrosa.
cambiarAltura :: Number -> Jovit -> Jovit
cambiarAltura cambioDeAltura jovit = UnJovit {nombre = nombre jovit, fuerza = fuerza jovit, esDeLaComarca = esDeLaComarca jovit, estatura = estatura jovit + cambioDeAltura}

-- Version copada :)
cambiarAltura2 :: Number -> Jovit -> Jovit
cambiarAltura2 cambioDeAltura jovit = 
    jovit{estatura = estatura jovit + cambioDeAltura}


perderHabla :: Jovit -> Jovit
perderHabla jovit 
    | tienePocaReputacion jovit = reducirNombre jovit
    | otherwise = jovit

tienePocaReputacion :: Jovit -> Bool
tienePocaReputacion jovit = reputacion jovit < 700

reducirNombre :: Jovit -> Jovit
reducirNombre jovit = jovit{nombre = drop 4 (nombre jovit)}

inimputabilizar :: Jovit -> Jovit
inimputabilizar jovit = jovit{nombre = agregarInimputabilidad jovit, esDeLaComarca = True, estatura = estatura jovit + 10, fuerza = fuerza jovit / 2}

agregarInimputabilidad :: Jovit -> String
agregarInimputabilidad jovit = nombre jovit ++ " sos inimputable"


type Censo = [Jovit]

amanecioConElCenso :: Censo -> Jovit
amanecioConElCenso censo = head censo

seCansoDeEsperar :: Censo -> String
seCansoDeEsperar censo = nombre (last censo)

participacion :: Censo -> Number
participacion censo = length censo

sumarAlCenso :: Jovit -> Censo -> Censo
sumarAlCenso jovit censo = jovit : censo

agregarAlCenso :: Censo -> Number -> Censo
agregarAlCenso censo fuerza = sumarAlCenso (nuevoCensado censo fuerza) censo

nuevoCensado :: Censo -> Number -> Jovit
nuevoCensado censo fuerza = UnJovit{nombre = nombre (last censo), estatura = estatura (head censo), fuerza = fuerza, esDeLaComarca = True } 