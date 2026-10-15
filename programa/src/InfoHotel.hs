-- |
-- Modulo: InfoHotel
-- Objetivo: leer desde un archivo la informacion de la empresa gestora
-- (nombre, cedula juridica, sitio web, telefono, pais y provincia).
module InfoHotel (cargarInfoHotel, parseInfoHotel, infoPorDefecto) where

import Tipos
import Utilidades

-- | Objetivo: informacion usada cuando el archivo no existe o esta mal formado.
infoPorDefecto :: InfoHotel
infoPorDefecto = InfoHotel "(sin informacion)" "-" "-" "-" "-" "-"

-- | Objetivo: convertir la primera linea util del archivo en InfoHotel.
-- Entradas: contenido del archivo con el formato
--   nombre,cedula juridica,sitio web,telefono,pais,provincia
-- Salidas: Just InfoHotel si hay exactamente 6 campos no vacios, Nothing si no.
parseInfoHotel :: String -> Maybe InfoHotel
parseInfoHotel contenido = case lineasUtiles contenido of
  (l : _) -> case map trim (splitarPor ',' l) of
    [n, c, w, t, p, pr] | all (not . null) [n, c, w, t, p, pr] ->
      Just (InfoHotel n c w t p pr)
    _ -> Nothing
  [] -> Nothing

-- | Objetivo: cargar la informacion del hotel al iniciar el programa.
-- Entradas: ruta del archivo. Salidas: InfoHotel (la del archivo o la
-- informacion por defecto, avisando del problema; nunca falla).
cargarInfoHotel :: FilePath -> IO InfoHotel
cargarInfoHotel ruta = do
  r <- leerArchivo ruta
  case r of
    Left err -> aviso err
    Right c  -> maybe (aviso ("Formato invalido en " ++ ruta)) return (parseInfoHotel c)
  where
    aviso msg = putStrLn ("Aviso: " ++ msg ++ ". Se usara informacion por defecto.")
                >> return infoPorDefecto