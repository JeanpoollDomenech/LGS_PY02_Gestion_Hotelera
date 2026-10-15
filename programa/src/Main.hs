-- Modulo Main
-- Objetivo punto de entrada
-- configura la consola y carga la informacion de la
-- empresa desde datos/hotel.txt y recupera los datos guardados si existen y abre el Menu Principal
-- Debe ejecutarse desde la carpeta programa/ para que la ruta relativa funcione
module Main (main) where

import InfoHotel (cargarInfoHotel)
import Menus (menuPrincipal)
import Persistencia (cargarEstado)
import System.IO

-- Objetivo iniciar el sistema
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  inf <- cargarInfoHotel "datos/hotel.txt"
  hotel <- cargarEstado inf
  menuPrincipal hotel
