-- |
-- Modulo: Menus
-- Objetivo: Menu Principal, Menu Administrativo y Menu General. Cada opcion
-- recibe el Hotel actual y devuelve el Hotel actualizado; las que modifican
-- datos guardan el estado en disco (persistencia) con 'guardando'.
module Menus (menuPrincipal) where

import Disponibilidad
import Estadisticas
import Facturacion
import Habitaciones
import Persistencia
import Reservas
import Tarifas
import Tipos
import Utilidades

-- | Objetivo: Menu Principal (administrativo, general o salir).
-- Entradas: Hotel. Salidas: IO () (termina al elegir salir).
menuPrincipal :: Hotel -> IO ()
menuPrincipal h = do
  putStrLn "\n===== SISTEMA DE GESTION HOTELERA ====="
  putStrLn "1. Opciones administrativas"
  putStrLn "2. Opciones generales"
  putStrLn "3. Salir"
  op <- leerOpcion "Seleccione una opcion: "
  case op of
    Just 1 -> menuAdmin h >>= menuPrincipal
    Just 2 -> menuGeneral h >>= menuPrincipal
    Just 3 -> putStrLn "Gracias por usar el sistema. ¡Hasta pronto!"
    _      -> putStrLn "Opcion invalida." >> menuPrincipal h

-- | Objetivo: Menu Administrativo.
-- Entradas: Hotel. Salidas: Hotel actualizado al regresar con la opcion 0.
menuAdmin :: Hotel -> IO Hotel
menuAdmin h = do
  putStrLn "\n--- MENU ADMINISTRATIVO ---"
  putStrLn "1. Cargar tipos de habitaciones"
  putStrLn "2. Asignar cantidad de habitaciones por tipo"
  putStrLn "3. Ver habitaciones"
  putStrLn "4. Carga de tarifas"
  putStrLn "5. Consultar reservaciones"
  putStrLn "6. Consultar estado de ocupacion"
  putStrLn "7. Estadisticas de ocupacion"
  putStrLn "0. Regresar al Menu Principal"
  op <- leerOpcion "Seleccione una opcion: "
  case op of
    Just 1 -> cargarTipos h >>= guardando >>= menuAdmin
    Just 2 -> asignarCantidades h >>= guardando >>= menuAdmin
    Just 3 -> verHabitaciones h >> menuAdmin h
    Just 4 -> cargarTarifas h >>= guardando >>= menuAdmin
    Just 5 -> historialReservas h >> menuAdmin h
    Just 6 -> estadoOcupacion h >> menuAdmin h
    Just 7 -> menuEstadisticas h >> menuAdmin h
    Just 0 -> return h
    _      -> putStrLn "Opcion invalida." >> menuAdmin h

-- | Objetivo: Menu General (usuario normal).
-- Entradas: Hotel. Salidas: Hotel actualizado al regresar con la opcion 0.
menuGeneral :: Hotel -> IO Hotel
menuGeneral h = do
  putStrLn "\n--- MENU GENERAL ---"
  putStrLn "1. Ver disponibilidad"
  putStrLn "2. Reservacion"
  putStrLn "3. Anular reservacion"
  putStrLn "4. Facturar reservacion"
  putStrLn "0. Regresar al Menu Principal"
  op <- leerOpcion "Seleccione una opcion: "
  case op of
    Just 1 -> verDisponibilidad h >> menuGeneral h
    Just 2 -> hacerReserva h >>= guardando >>= menuGeneral
    Just 3 -> anularReserva h >>= guardando >>= menuGeneral
    Just 4 -> facturarReserva h >>= guardando >>= menuGeneral
    Just 0 -> return h
    _      -> putStrLn "Opcion invalida." >> menuGeneral h