-- |
-- Modulo: Estadisticas
-- Objetivo: las cinco estadisticas de ocupacion del menu administrativo.
-- Las funciones de calculo son puras; las de menu solo imprimen resultados.
-- En todas se ignoran las reservaciones canceladas.
module Estadisticas
  ( menuEstadisticas, huespedesPorMes, montoRecaudado, conteoPorDia, top3Dias
  ) where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Data.Time.Calendar (DayOfWeek, dayOfWeek, toGregorian)
import Disponibilidad
import Tarifas (noches)
import Text.Printf (printf)
import Tipos
import Utilidades

-- | Objetivo: reservas no canceladas. Entradas: Hotel. Salidas: lista de Reserva.
vigentes :: Hotel -> [Reserva]
vigentes = filter ((/= Cancelada) . estadoRes) . reservas

-- | Objetivo: total de huespedes por (ano, mes) de la fecha de entrada.
-- Entradas: reservas. Salidas: Map (ano, mes) -> huespedes (ordenado por fecha).
huespedesPorMes :: [Reserva] -> Map.Map (Integer, Int) Int
huespedesPorMes = foldr (\r -> Map.insertWith (+) (clave r) (totalHuespedes r)) Map.empty
  where clave r = let (a, m, _) = toGregorian (entrada r) in (a, m)

-- | Objetivo: monto total recaudado incluyendo el 13% de impuesto.
-- Entradas: reservas (ya sin canceladas). Salidas: suma de monto * 1.13.
montoRecaudado :: [Reserva] -> Double
montoRecaudado = sum . map ((* (1 + tasaIVA)) . montoRes)

-- | Objetivo: contar habitaciones ocupadas por dia de la semana
-- (cada noche de cada habitacion cuenta como una ocupacion).
-- Entradas: reservas. Salidas: Map (numero de dia, 1=lunes..7=domingo) -> cantidad.
conteoPorDia :: [Reserva] -> Map.Map Int Int
conteoPorDia = foldr agregar Map.empty
  where
    agregar r m = foldr (\d -> Map.insertWith (+) (fromEnum (dayOfWeek d)) (length (ocupaciones r)))
                        m (noches (entrada r) (salida r))

-- | Objetivo: los tres dias de la semana con mas ocupaciones.
-- Entradas: conteo por dia. Salidas: hasta 3 pares (dia, cantidad) de mayor a
-- menor; en empate se conserva el orden lunes a domingo.
top3Dias :: Map.Map Int Int -> [(DayOfWeek, Int)]
top3Dias = take 3 . sortBy (comparing (Down . snd)) . map (\(k, v) -> (toEnum k, v)) . Map.toList

-- | Objetivo: submenu de estadisticas de ocupacion.
-- Entradas: Hotel. Salidas: IO () (vuelve al menu administrativo con la opcion 0).
menuEstadisticas :: Hotel -> IO ()
menuEstadisticas h = do
  putStrLn "\n--- ESTADISTICAS DE OCUPACION ---"
  putStrLn "1. Total de huespedes por mes-anio"
  putStrLn "2. Habitaciones ocupadas en un dia"
  putStrLn "3. Total de habitaciones no ocupadas en un dia"
  putStrLn "4. Monto recaudado con impuestos"
  putStrLn "5. Top 3 de dias de la semana con mas ocupaciones"
  putStrLn "0. Regresar"
  op <- leerOpcion "Seleccione una opcion: "
  case op of
    Just 1 -> porMes >> menuEstadisticas h
    Just 2 -> consultarDia h (\d -> putStrLn "Habitaciones ocupadas:"
                                    >> mostrarOcupadas (fst (ocupacionDia d h))) >> menuEstadisticas h
    Just 3 -> consultarDia h (\d -> let libres = snd (ocupacionDia d h)
                                    in putStrLn ("Habitaciones no ocupadas: " ++ show (length libres)
                                                 ++ " de " ++ show (length (habitaciones h))))
              >> menuEstadisticas h
    Just 4 -> putStrLn ("Monto recaudado con impuestos (sin canceladas): "
                        ++ formatoMonto (montoRecaudado (vigentes h))) >> menuEstadisticas h
    Just 5 -> top >> menuEstadisticas h
    Just 0 -> return ()
    _      -> putStrLn "Opcion invalida." >> menuEstadisticas h
  where
    porMes
      | null (vigentes h) = putStrLn "No hay reservaciones."
      | otherwise = mapM_ (\((a, m), n) -> putStrLn (printf "%04d-%02d: %d huespedes" a m n))
                          (Map.toList (huespedesPorMes (vigentes h)))
    top
      | null (vigentes h) = putStrLn "No hay reservaciones."
      | otherwise = do
          putStrLn "Dias con mas habitaciones ocupadas:"
          mapM_ (\(i, (d, n)) -> putStrLn ("  " ++ show i ++ ". " ++ nombreDia d ++ ": " ++ show n))
                (zip [1 :: Int ..] (top3Dias (conteoPorDia (vigentes h))))