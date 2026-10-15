-- Modulo Disponibilidad
-- Objetivo determinar que habitaciones estan libres u ocupadas en un dia o en
-- un rango de fechas Una habitacion esta ocupada si tiene una reservacion no
-- cancelada activa o facturada que cubra la noche consultada
module Disponibilidad
  ( ocupa, ocupacionDia, habitacionLibre
  , verDisponibilidad, estadoOcupacion
  , consultarDia, mostrarLibres, mostrarOcupadas
  ) where

import Data.List (find)
import Data.Time.Calendar (Day)
import Tipos
import Utilidades

-- Objetivo saber si una reservacion esta vigente en un dia
-- Entradas dia y reserva
-- Salidas True si no esta cancelada y
-- entrada menor o igual al dia y el dia menor que salida
ocupa :: Day -> Reserva -> Bool
ocupa d r = estadoRes r /= Cancelada && entrada r <= d && d < salida r

-- Objetivo separar las habitaciones de un dia en ocupadas y libres
-- Entradas dia y Hotel
-- Salidas una pareja con las ocupadas junto al id de su reserva y las libres
-- ambas ordenadas por id
ocupacionDia :: Day -> Hotel -> ([(Habitacion, Int)], [Habitacion])
ocupacionDia d h = foldr clasificar ([], []) (habitaciones h)
  where
    clasificar hb (oc, li) = case reservaDe hb of
      Just r  -> ((hb, idRes r) : oc, li)
      Nothing -> (oc, hb : li)
    reservaDe hb = find (\r -> ocupa d r && any ((== idHab hb) . idHabOc) (ocupaciones r))
                        (reservas h)

-- Objetivo saber si una habitacion esta libre durante todas las noches de
-- un rango desde la entrada hasta antes de la salida
-- Entradas Hotel entrada salida y habitacion
-- Salidas True si no choca con
-- ninguna reservacion no cancelada
habitacionLibre :: Hotel -> Day -> Day -> Habitacion -> Bool
habitacionLibre h e s hb = not (any choca (reservas h))
  where
    choca r = estadoRes r /= Cancelada && entrada r < s && e < salida r
              && any ((== idHab hb) . idHabOc) (ocupaciones r)

-- Objetivo pedir un dia y ejecutar una accion con el
-- Restricciones requiere habitaciones generadas
-- Entradas Hotel y accion a ejecutar con la fecha
-- Salidas IO
consultarDia :: Hotel -> (Day -> IO ()) -> IO ()
consultarDia h accion
  | null (habitaciones h) = putStrLn "Aun no se han generado habitaciones."
  | otherwise = pedirFecha "Dia a consultar (AAAA-MM-DD): " >>= accion

-- Objetivo imprimir las habitaciones libres
-- Entradas lista de habitaciones
-- Salidas IO
mostrarLibres :: [Habitacion] -> IO ()
mostrarLibres [] = putStrLn "  (ninguna)"
mostrarLibres hs = mapM_ (\hb -> putStrLn ("  Habitacion " ++ show (idHab hb) ++ " (" ++ tipoHab hb ++ ")")) hs

-- Objetivo imprimir las habitaciones ocupadas con su codigo de reserva
-- Entradas lista de pares con la habitacion y el id de su reserva
-- Salidas IO
mostrarOcupadas :: [(Habitacion, Int)] -> IO ()
mostrarOcupadas [] = putStrLn "  (ninguna)"
mostrarOcupadas ps = mapM_ (\(hb, r) -> putStrLn ("  Habitacion " ++ show (idHab hb)
                       ++ " (" ++ tipoHab hb ++ ") - reserva #" ++ show r)) ps

-- Objetivo opcion general Ver disponibilidad de un dia
-- Entradas Hotel
-- Salidas imprime las habitaciones disponibles
verDisponibilidad :: Hotel -> IO ()
verDisponibilidad h = consultarDia h $ \d -> do
  putStrLn "Habitaciones disponibles:"
  mostrarLibres (snd (ocupacionDia d h))

-- Objetivo opcion administrativa Consultar estado de ocupacion
-- Entradas Hotel
-- Salidas imprime habitaciones disponibles y ocupadas
estadoOcupacion :: Hotel -> IO ()
estadoOcupacion h = consultarDia h $ \d -> do
  let (oc, li) = ocupacionDia d h
  putStrLn "Habitaciones disponibles:" >> mostrarLibres li
  putStrLn "Habitaciones ocupadas:"    >> mostrarOcupadas oc
