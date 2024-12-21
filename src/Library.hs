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
} Deriving (Show,Eq)

av1 :: Aventrurero
av1 = UnAventurero "Lucas" 10 100 True conformista

av2 :: Aventrurero
av2 = UnAventurero "Matias" 20 50 False valiente

av3 :: Aventrurero
av3 = UnAventurero "Pepe" 30 70 True lightPacker

--type Criterio = Aventurero -> Encuentro -> Bool
type Criterio = Aventurero -> Encuentro -> Bool

conformista :: Criterio
conformista _ _ = True

valiente :: Criterio
valiente aventurero encuentro
    | aventurero = 

--estaConforme :: Aventrurero -> Encuentro -> Bool
--estaConforme aventurero encuentro = 

--2a

type Aventureros = [Aventurero]

aventureros :: Aventureros
aventureros = [av1,av2,av3]

nombreMasDe5 :: Aventureros -> Bool
nombreMasDe5 aventureros = any masDe5 aventureros

masDe5 :: Aventurero -> Bool
masDe5 aventrurero = ( length nombre aventurero) > 5
--2b

cargaTotal :: Aventureros -> Number
cargaTotal aventureros = sum (filter cargaPar) aventureros

cargaPar :: Aventurero -> Bool
cargaPar aventurero = even (carga aventurero)

--3

type Encuentro = Aventurero -> Aventurero 

encuentroConPersonaje :: Encuentro -> Aventurero -> Aventurero
encuentroConPersonaje personaje aventrurero = reducirCarga 1 . personaje aventurero 

curandero :: Encuentro
curandero  aventrurero = aumentarSalud 20 . reducirCarga (carga aventrurero / 2)

inspirador :: Encuentro
inspirador = aumentarSalud 10 . otorgarCoraje

embaucador :: Encuentro
embaucador = cambiarCriterio (lightPacker) . aumentarSaludPorcentual (-50). reducirCarga (-10) . (not otorgarCoraje)

otorgarCoraje :: Aventrurero -> Aventurero
otorgarCoraje aventurero = aventrurero{coraje = True}

reducirCarga :: Number -> Aventrurero -> Aventurero
reducirCarga num aventurero = aventrurero{carga = carga aventrurero - num}

aumentarSaludPorcentual :: Number -> Aventurero -> Aventurero
aumentarSaludPorcentual num aventrurero = aventrurero{salud = salud aventrurero + (salud aventrurero) * (num/100)}

cambiarCriterio :: Criterio -> Aventurero -> Aventurero
cambiarCriterio criterioNuevo aventrurero = aventrurero{criterio = criterioNuevo }

--4

type Encuentros = [Encuentro]

encuentros :: Encuentros
encuentros = [curandero,inspirador,embaucador]


