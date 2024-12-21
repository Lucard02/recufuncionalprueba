module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Aventurero = UnAventurero{
    nombre :: String,
    carga :: Number,
    salud :: Number,
    coraje :: Bool,
    criterio :: Criterio 
} deriving (Show,Eq)

av1 :: Aventurero
av1 = UnAventurero "Lucas" 10 100 True conformista

av2 :: Aventurero
av2 = UnAventurero "Matias" 20 50 False valiente 

av3 :: Aventurero
av3 = UnAventurero "Pepe" 30 70 True (lightPacker 15)

--type Criterio = Aventurero -> Encuentro -> Bool
type Criterio = Aventurero -> Encuentro -> Bool

conformista :: Criterio
conformista _ _ = True

valiente :: Criterio
valiente aventurero personaje
    | (coraje (encuentroConPersonaje aventurero personaje) == True || 50 < salud (encuentroConPersonaje aventurero personaje)) = True
    | otherwise = False

lightPacker :: Number -> Criterio
lightPacker umbral aventurero personaje
    |  umbral < carga (encuentroConPersonaje aventurero personaje) = True
    | otherwise = False

estaConforme :: Aventurero -> Encuentro -> Bool
estaConforme aventurero personaje = (criterio aventurero) aventurero personaje
    

--2a

type Aventureros = [Aventurero]

aventureros :: Aventureros
aventureros = [av1,av2,av3]

nombreMasDe5 :: Aventureros -> Bool
nombreMasDe5 aventureros = any masDe5 aventureros

masDe5 :: Aventurero -> Bool
masDe5 aventurero = length (nombre aventurero) > 5
--2b

cargaTotal :: Aventureros -> Number
cargaTotal aventureros = sum (map carga (filter cargaPar aventureros))

cargaPar :: Aventurero -> Bool
cargaPar aventurero = even (carga aventurero)

--3

type Encuentro = Aventurero -> Aventurero 

encuentroConPersonaje :: Encuentro -> Aventurero -> Aventurero
encuentroConPersonaje personaje aventurero = (reducirCarga 1 . personaje) aventurero 

curandero :: Encuentro
curandero  aventurero = (aumentarSaludPorcentual 20 . reducirCarga (carga aventurero / 2)) aventurero

inspirador :: Encuentro
inspirador = aumentarSaludPorcentual 10 . otorgarCoraje

embaucador :: Encuentro
embaucador = cambiarCriterio (lightPacker 10) . aumentarSaludPorcentual (-50). reducirCarga (-10) . quitarCoraje

otorgarCoraje :: Aventurero -> Aventurero
otorgarCoraje aventurero = aventurero{coraje = True}

quitarCoraje :: Aventurero -> Aventurero
quitarCoraje aventurero = aventurero{coraje = False}

reducirCarga :: Number -> Aventurero -> Aventurero
reducirCarga num aventurero = aventurero{carga = carga aventurero - num}

aumentarSaludPorcentual :: Number -> Aventurero -> Aventurero
aumentarSaludPorcentual num aventrurero = aventrurero{salud = salud aventrurero + (salud aventrurero) * (num/100)}

cambiarCriterio :: Criterio -> Aventurero -> Aventurero
cambiarCriterio criterioNuevo aventrurero = aventrurero{criterio = criterioNuevo }

--4

type Encuentros = [Encuentro]

encuentros :: Encuentros
encuentros = [curandero,inspirador,embaucador]


