-- |
-- Modulo: Tipos
-- Objetivo: definir los tipos de datos del sistema y el estado global (Hotel).
-- Todo el estado del programa vive en un valor 'Hotel' que se pasa de funcion
-- en funcion (no se usan variables globales mutables).
module Tipos where

import qualified Data.Map as Map
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime)

-- | Tipo de habitacion cargado desde archivo.
data TipoHab = TipoHab
  { nombreTipo   :: String   -- ^ Nombre unico del tipo (no se repite)
  , descripcion  :: String   -- ^ Descripcion libre
  , maxHuespedes :: Int      -- ^ Maximo de huespedes por habitacion (> 0)
  } deriving (Show, Read, Eq)

-- | Habitacion fisica del hotel, con identificador autogenerado.
data Habitacion = Habitacion
  { idHab   :: Int           -- ^ Identificador autogenerado (1, 2, 3...)
  , tipoHab :: String        -- ^ Nombre de su tipo
  } deriving (Show, Read, Eq)

-- | Estados posibles de una reservacion.
data EstadoReserva = Activa | Facturada | Cancelada
  deriving (Show, Read, Eq)

-- | Detalle de una habitacion dentro de una reservacion.
data Ocupacion = Ocupacion
  { idHabOc   :: Int         -- ^ Id de la habitacion reservada
  , tipoOc    :: String      -- ^ Tipo de la habitacion
  , adultosOc :: Int         -- ^ Adultos alojados en esa habitacion
  , ninosOc   :: Int         -- ^ Ninos alojados en esa habitacion
  } deriving (Show, Read, Eq)

-- | Reservacion completa.
data Reserva = Reserva
  { idRes        :: Int             -- ^ Identificador autogenerado
  , nombreRes    :: String          -- ^ Nombre de quien reserva
  , fechaHoraRes :: LocalTime       -- ^ Momento en que se hizo la reservacion
  , entrada      :: Day             -- ^ Fecha de ingreso
  , salida       :: Day             -- ^ Fecha de salida (esa noche no se ocupa)
  , estadoRes    :: EstadoReserva   -- ^ Activa, Facturada o Cancelada
  , ocupaciones  :: [Ocupacion]     -- ^ Habitaciones reservadas
  , montoRes     :: Double          -- ^ Monto sin impuesto
  } deriving (Show, Read, Eq)

-- | Factura emitida para una reservacion.
data Factura = Factura
  { idFac        :: Int
  , idResFac     :: Int
  , fechaHoraFac :: LocalTime
  , subtotalFac  :: Double
  , impuestoFac  :: Double
  , totalFac     :: Double
  } deriving (Show, Read, Eq)

-- | Informacion de la empresa gestora (se lee de un archivo).
data InfoHotel = InfoHotel
  { ihNombre    :: String
  , ihCedula    :: String
  , ihSitioWeb  :: String
  , ihTelefono  :: String
  , ihPais      :: String
  , ihProvincia :: String
  } deriving (Show, Read, Eq)

-- | Estado completo del sistema.
data Hotel = Hotel
  { info         :: InfoHotel
  , tipos        :: [TipoHab]
  , habitaciones :: [Habitacion]
  , tarifas      :: Map.Map Int Double  -- ^ codigo de tarifa (1..8) -> monto por persona
  , reservas     :: [Reserva]
  , facturas     :: [Factura]
  , sigRes       :: Int                 -- ^ Proximo id de reservacion
  , sigFac       :: Int                 -- ^ Proximo id de factura
  } deriving (Show, Read, Eq)

-- | Tasa del impuesto de ventas (13%).
tasaIVA :: Double
tasaIVA = 0.13

-- | Objetivo: crear el estado inicial del sistema (sin datos).
-- Entradas: informacion de la empresa. Salidas: Hotel vacio.
hotelVacio :: InfoHotel -> Hotel
hotelVacio i = Hotel
  { info = i, tipos = [], habitaciones = [], tarifas = Map.empty
  , reservas = [], facturas = [], sigRes = 1, sigFac = 1 }

-- | Objetivo: total de adultos de una reservacion.
-- Entradas: Reserva. Salidas: suma de adultos de todas sus habitaciones.
totalAdultos :: Reserva -> Int
totalAdultos = sum . map adultosOc . ocupaciones

-- | Objetivo: total de ninos de una reservacion.
-- Entradas: Reserva. Salidas: suma de ninos de todas sus habitaciones.
totalNinos :: Reserva -> Int
totalNinos = sum . map ninosOc . ocupaciones

-- | Objetivo: total de huespedes (adultos + ninos) de una reservacion.
-- Entradas: Reserva. Salidas: cantidad entera de personas.
totalHuespedes :: Reserva -> Int
totalHuespedes r = totalAdultos r + totalNinos r