-- |
-- Modulo: Tarifas
-- Objetivo: cargar las 8 tarifas desde archivo y calcular el monto de una
-- estadia segun temporada, tipo de dia y tipo de huesped.
module Tarifas
  ( cargarTarifas, parseTarifas, codigoTarifa, tarifaDia, noches, calcularMonto
  ) where

import qualified Data.Map as Map
import Data.List (sort)
import Data.Time.Calendar (Day, DayOfWeek (..), addDays, dayOfWeek, toGregorian)
import Tipos
import Utilidades

-- | Objetivo: convertir el contenido del archivo de tarifas en un Map.
-- Entradas: contenido con 8 lineas "id,monto" (ids 1 a 8, sin repetir).
-- Salidas: Right (Map id monto) si todo es valido, o Left con el motivo.
-- La carga es "todo o nada".
parseTarifas :: String -> Either String (Map.Map Int Double)
parseTarifas contenido
  | length lineas /= 8 =
      Left ("El archivo debe tener exactamente 8 lineas (tiene " ++ show (length lineas) ++ ").")
  | otherwise = do
      pares <- mapM parseLinea lineas
      if sort (map fst pares) == [1 .. 8]
        then Right (Map.fromList pares)
        else Left "Los codigos de tarifa deben ser exactamente 1, 2, 3, 4, 5, 6, 7 y 8, sin repetir."
  where
    lineas = lineasUtiles contenido
    parseLinea l = case splitarPor ',' l of
      [a, b] | Just i <- parseEntero (trim a), Just m <- parseMonto (trim b) -> Right (i, m)
      _ -> Left ("Linea invalida: \"" ++ l ++ "\"")

-- | Objetivo: opcion "Carga de Tarifas". Reemplaza todas las tarifas anteriores.
-- Entradas: Hotel. Salidas: Hotel con las tarifas nuevas (sin cambios si hay error).
cargarTarifas :: Hotel -> IO Hotel
cargarTarifas h = do
  ruta <- leerLinea "Ruta del archivo de tarifas: "
  r <- leerArchivo ruta
  case r >>= parseTarifas of
    Left err -> putStrLn err >> putStrLn "No se modificaron las tarifas." >> return h
    Right m  -> putStrLn "Tarifas cargadas correctamente (se reemplazaron las anteriores)."
                >> return h { tarifas = m }

-- | Objetivo: obtener el codigo de tarifa (1 a 8) de una noche.
-- Verde = abril a octubre (base 0); Alta = noviembre a marzo (base 4).
-- Fin de semana = viernes y sabado (+2); adulto = +1, nino = +2.
-- Entradas: fecha de la noche y True si es adulto. Salidas: codigo 1..8.
codigoTarifa :: Day -> Bool -> Int
codigoTarifa dia esAdulto = base + finde + tipo
  where
    (_, mes, _) = toGregorian dia
    base  = if mes >= 4 && mes <= 10 then 0 else 4
    finde = if dayOfWeek dia `elem` [Friday, Saturday] then 2 else 0
    tipo  = if esAdulto then 1 else 2

-- | Objetivo: monto por persona de una noche.
-- Entradas: tarifas, fecha, True si es adulto. Salidas: monto (0 si no existe).
tarifaDia :: Map.Map Int Double -> Day -> Bool -> Double
tarifaDia m d esAdulto = Map.findWithDefault 0 (codigoTarifa d esAdulto) m

-- | Objetivo: lista de noches de una estadia (la de la fecha de salida no cuenta).
-- Entradas: fecha de entrada y de salida. Salidas: dias [entrada, salida).
noches :: Day -> Day -> [Day]
noches e s = takeWhile (< s) (iterate (addDays 1) e)

-- | Objetivo: monto total (sin IVA) de una reservacion.
-- Entradas: tarifas, entrada, salida y habitaciones con sus huespedes.
-- Salidas: suma, por noche y habitacion, de adultos*tarifa adulto + ninos*tarifa nino.
calcularMonto :: Map.Map Int Double -> Day -> Day -> [Ocupacion] -> Double
calcularMonto m e s ocs = sum (map costoNoche (noches e s))
  where
    costoNoche d = sum (map (costoHab d) ocs)
    costoHab d o = fromIntegral (adultosOc o) * tarifaDia m d True
                 + fromIntegral (ninosOc o)   * tarifaDia m d False