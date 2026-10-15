-- Modulo Persistencia
-- Objetivo mantener los datos entre ejecuciones guardando el estado completo
-- del sistema Hotel en un archivo de texto y recuperandolo al iniciar
-- El formato es el de show y read de Haskell ya que los tipos derivan Show y Read
module Persistencia (archivoEstado, guardarEstado, guardando, cargarEstado) where

import Control.Exception (IOException, try)
import System.IO
import Text.Read (readMaybe)
import Tipos
import Utilidades (leerArchivo)

-- Ruta del archivo donde se guarda el estado relativa a programa/
archivoEstado :: FilePath
archivoEstado = "datos/estado.dat"

-- Objetivo escribir el estado completo en el archivo de persistencia
-- Entradas Hotel
-- Salidas IO si no se puede escribir avisa y el
-- programa continua funcionando solo con datos en memoria
guardarEstado :: Hotel -> IO ()
guardarEstado h = do
  r <- try escribir :: IO (Either IOException ())
  either (const (putStrLn "Aviso: no se pudo guardar el estado en disco.")) return r
  where
    escribir = withFile archivoEstado WriteMode $ \f -> do
      hSetEncoding f utf8
      hPutStr f (show h)

-- Objetivo guardar el estado y devolverlo para poder encadenarlo con otras acciones
-- Entradas Hotel
-- Salidas el mismo Hotel ya guardado
guardando :: Hotel -> IO Hotel
guardando h = guardarEstado h >> return h

-- Objetivo recuperar el estado guardado al iniciar el programa
-- Entradas informacion de la empresa leida de hotel.txt siempre se usa la
-- mas reciente del archivo no la guardada
-- Salidas el Hotel guardado o un Hotel vacio si el archivo no existe o esta
-- danado en ese caso avisa
cargarEstado :: InfoHotel -> IO Hotel
cargarEstado inf = do
  r <- leerArchivo archivoEstado
  case r of
    Left _ -> return (hotelVacio inf)       -- primera ejecucion no hay datos
    Right c -> case readMaybe c of
      Just h  -> do
        putStrLn ("Datos anteriores recuperados: " ++ show (length (tipos h)) ++ " tipos, "
                  ++ show (length (habitaciones h)) ++ " habitaciones, "
                  ++ show (length (reservas h)) ++ " reservaciones.")
        return h { info = inf }
      Nothing -> do
        putStrLn "Aviso: el archivo de datos esta danado; se inicia con datos vacios."
        return (hotelVacio inf)
