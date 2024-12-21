module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Aventurero = UnAventurero{
    nombre :: String,
    carga :: Number,
    salud :: Number,
    coraje :: Bool,
    criterioDeSeleccionDeEncuentros :: Encuentro 
} Deriving (Show,Eq)

av1 :: Aventrurero
av1 = UnAventurero "Lucas" 10 100 True ...

av2 :: Aventrurero
av2 = UnAventurero "Matias" 20 50 False ...

av3 :: Aventrurero
av3 = UnAventurero "Pepe" 30 70 True ...

type Criterio = Aventurero -> Bool

conformista :: Criterio
conformista 

valiente :: 

--2a

type Aventureros = [Aventrurero]

aventrureros :: aventrureros
aventrureros = [av1,av2]

nombreMasDe5 :: Aventrureros -> Bool
nombreMasDe5 aventrureros = any masDe5 aventrureros

masDe5 :: Aventrurero -> Bool
masDe5 aventrurero = ( length nombre aventrurero) > 5
--2b

cargaTotal :: Aventrureros -> Number
cargaTotal aventrureros = sum (filter cargaPar) aventrureros

cargaPar :: Aventrurero -> Bool
cargaPar aventurero = even (carga aventrurero)

--3

type Encuentro = Aventrurero -> Aventrurero 

curandero :: Encuentro
curandero  aventrurero = aumentarSalud 20 . reducirCarga (carga aventrurero / 2)

inspirador :: Encuentro
inspirador = aumentarSalud 10 . otorgarCoraje

embaucador :: Encuentro
embaucador = cambiarCriterio (lightPacker) . aumentarSaludPorcentual (-50). reducirCarga (-10) . (not otorgarCoraje)

otorgarCoraje :: Aventrurero -> Aventrurero
otorgarCoraje aventurero = aventrurero{coraje = True}

reducirCarga :: Number -> Aventrurero -> Aventrurero
reducirCarga num aventurero = aventrurero{carga = carga aventrurero - num}

aumentarSaludPorcentual :: Number -> Aventrurero -> Aventrurero
aumentarSaludPorcentual num aventrurero = aventrurero{salud = salud aventrurero + (salud aventrurero) * (num/100)}

cambiarCriterio :: Criterio -> Aventrurero -> Aventrurero
cambiarCriterio criterioNuevo aventrurero = aventrurero{criterio = criterioNuevo }
