-- |
-- Modulo: Reservas
-- Objetivo: crear reservaciones (con comprobante), anularlas y consultar el
-- historial.
module Reservas
  ( hacerReserva, anularReserva, historialReservas
  , Solicitud (..), asignarHabitaciones, formatoReserva, formatoHora
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.List (find, intercalate)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime, getZonedTime, zonedTimeToLocalTime)
import Disponibilidad
import Tarifas
import Tipos
import Utilidades

-- | Solicitud de una habitacion dentro de una reservacion (antes de asignarle id).
data Solicitud = Solicitud
  { solTipo    :: String
  , solAdultos :: Int
  , solNinos   :: Int
  }

-- | Objetivo: formatear fecha y hora como AAAA-MM-DD HH:MM:SS.
-- Entradas: LocalTime. Salidas: String.
formatoHora :: LocalTime -> String
formatoHora = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Objetivo: texto con todos los datos de una reservacion (sirve para el
-- comprobante, el historial y el detalle de la factura).
-- Entradas: Reserva. Salidas: String de varias lineas.
formatoReserva :: Reserva -> String
formatoReserva r = intercalate "\n" $
  [ "Reservacion #" ++ show (idRes r)
  , "  A nombre de: " ++ nombreRes r
  , "  Fecha y hora de la reservacion: " ++ formatoHora (fechaHoraRes r)
  , "  Fecha de ingreso: " ++ showGregorian (entrada r)
      ++ " | Fecha de salida: " ++ showGregorian (salida r)
  , "  Adultos: " ++ show (totalAdultos r) ++ " | Ninos: " ++ show (totalNinos r)
  , "  Estado: " ++ show (estadoRes r)
  , "  Total (sin IVA): " ++ formatoMonto (montoRes r)
  , "  Habitaciones:" ]
  ++ map linea (ocupaciones r)
  where
    linea o = "    - Habitacion " ++ show (idHabOc o) ++ " (" ++ tipoOc o ++ "): "
              ++ show (adultosOc o) ++ " adultos, " ++ show (ninosOc o) ++ " ninos"

-- | Objetivo: asignar una habitacion libre concreta a cada solicitud.
-- Entradas: Hotel, entrada, salida y solicitudes.
-- Salidas: Right con las ocupaciones (ids distintos, la libre de menor id de
-- cada tipo) o Left indicando el tipo sin disponibilidad.
asignarHabitaciones :: Hotel -> Day -> Day -> [Solicitud] -> Either String [Ocupacion]
asignarHabitaciones h e s sols = reverse . snd <$> foldM paso ([], []) sols
  where
    paso (usadas, acc) sol =
      case filter candidata (habitaciones h) of
        []       -> Left ("No hay disponibilidad de habitaciones tipo '" ++ solTipo sol
                          ++ "' para las fechas indicadas.")
        (hb : _) -> Right ( idHab hb : usadas
                          , Ocupacion (idHab hb) (solTipo sol) (solAdultos sol) (solNinos sol) : acc )
      where
        candidata hb = tipoHab hb == solTipo sol
                       && idHab hb `notElem` usadas
                       && habitacionLibre h e s hb

-- | Objetivo: pedir dos fechas y exigir que la entrada sea anterior a la salida.
-- Salidas: (entrada, salida) validas.
pedirRango :: IO (Day, Day)
pedirRango = do
  e <- pedirFecha "Fecha de ingreso (AAAA-MM-DD): "
  s <- pedirFecha "Fecha de salida (AAAA-MM-DD): "
  if e < s then return (e, s)
  else putStrLn "La fecha de salida debe ser posterior a la de ingreso." >> pedirRango

-- | Objetivo: tipos que tienen al menos una habitacion generada.
tiposDisponibles :: Hotel -> [TipoHab]
tiposDisponibles h = filter (\t -> any ((== nombreTipo t) . tipoHab) (habitaciones h)) (tipos h)

-- | Objetivo: pedir los datos de una habitacion (tipo, adultos y ninos).
-- Restricciones: adultos + ninos >= 1 y no superar el maximo del tipo.
-- Entradas: Hotel y numero de habitacion. Salidas: Solicitud valida.
pedirSolicitud :: Hotel -> Int -> IO Solicitud
pedirSolicitud h i = do
  let ts = tiposDisponibles h
  putStrLn ("\nHabitacion " ++ show i ++ " - tipos disponibles:")
  mapM_ (\(n, t) -> putStrLn ("  " ++ show n ++ ". " ++ nombreTipo t ++ " - " ++ descripcion t
                              ++ " (max. " ++ show (maxHuespedes t) ++ " huespedes)"))
        (zip [1 :: Int ..] ts)
  n <- pedirEntero "Seleccione el tipo: " (\k -> k >= 1 && k <= length ts) "Numero de tipo invalido."
  let t = ts !! (n - 1)
  huespedes t
  where
    huespedes t = do
      a <- pedirEntero "  Cantidad de adultos: " (const True) "Ingrese un entero mayor o igual a 0."
      k <- pedirEntero "  Cantidad de ninos: "   (const True) "Ingrese un entero mayor o igual a 0."
      if a + k >= 1 && a + k <= maxHuespedes t
        then return (Solicitud (nombreTipo t) a k)
        else putStrLn ("  Cada habitacion debe tener entre 1 y " ++ show (maxHuespedes t)
                       ++ " huespedes. Intente de nuevo.") >> huespedes t

-- | Objetivo: opcion "Reservacion".
-- Restricciones: requiere habitaciones generadas y las 8 tarifas cargadas;
-- verifica disponibilidad por tipo antes de crear la reserva.
-- Entradas: Hotel. Salidas: Hotel con la nueva reserva (estado Activa) y el
-- contador de reservas incrementado; sin cambios si no hay disponibilidad.
hacerReserva :: Hotel -> IO Hotel
hacerReserva h
  | null (habitaciones h) =
      putStrLn "No se puede reservar: aun no se han generado habitaciones." >> return h
  | Map.size (tarifas h) /= 8 =
      putStrLn "No se puede reservar: aun no se han cargado las tarifas." >> return h
  | otherwise = do
      nombre <- pedirTexto "Nombre de quien reserva: "
      (e, s) <- pedirRango
      cant   <- pedirEntero "Cantidad de habitaciones a reservar: " (>= 1) "Debe reservar al menos una habitacion."
      sols   <- mapM (pedirSolicitud h) [1 .. cant]
      case asignarHabitaciones h e s sols of
        Left err  -> putStrLn err >> putStrLn "La reservacion no fue creada." >> return h
        Right ocs -> do
          ahora <- zonedTimeToLocalTime <$> getZonedTime
          let r = Reserva { idRes = sigRes h, nombreRes = nombre, fechaHoraRes = ahora
                          , entrada = e, salida = s, estadoRes = Activa, ocupaciones = ocs
                          , montoRes = calcularMonto (tarifas h) e s ocs }
          putStrLn "\n===== COMPROBANTE DE RESERVACION ====="
          putStrLn (formatoReserva r)
          return h { reservas = reservas h ++ [r], sigRes = sigRes h + 1 }

-- | Objetivo: opcion "Anular reservacion".
-- Restricciones: la reserva debe existir y estar Activa. Al pasar a Cancelada
-- sus habitaciones quedan libres, porque la disponibilidad ignora las canceladas.
-- Entradas: Hotel. Salidas: Hotel con la reserva cancelada (o sin cambios).
anularReserva :: Hotel -> IO Hotel
anularReserva h = do
  i <- pedirEntero "Codigo de la reservacion a anular: " (const True) "Ingrese un numero entero."
  case find ((== i) . idRes) (reservas h) of
    Nothing -> putStrLn "No existe una reservacion con ese codigo." >> return h
    Just r
      | estadoRes r /= Activa ->
          putStrLn ("La reservacion no esta activa (estado: " ++ show (estadoRes r) ++ ").") >> return h
      | otherwise -> do
          putStrLn ("Reservacion #" ++ show i ++ " anulada; sus habitaciones quedaron liberadas.")
          return h { reservas = map (\x -> if idRes x == i then x { estadoRes = Cancelada } else x)
                                    (reservas h) }

-- | Objetivo: opcion administrativa "Consultar reservaciones" (historial).
-- Entradas: Hotel. Salidas: imprime todas las reservaciones con su detalle.
historialReservas :: Hotel -> IO ()
historialReservas h
  | null (reservas h) = putStrLn "No hay reservaciones registradas."
  | otherwise = mapM_ (putStrLn . (++ "\n") . formatoReserva) (reservas h)