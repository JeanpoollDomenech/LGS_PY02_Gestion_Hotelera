-- Modulo Habitaciones
-- Objetivo cargar los tipos de habitacion desde archivo generar las
-- habitaciones del hotel una unica vez y mostrarlas
module Habitaciones
  ( cargarTipos, asignarCantidades, verHabitaciones
  , parseTipo, agregarTipos, generarHabitaciones
  ) where

import Data.Char (toLower)
import Tipos
import Utilidades

-- Objetivo convertir una linea con nombre descripcion y maximo en un TipoHab
-- Entradas linea de texto
-- la descripcion puede contener comas asi que el nombre es
-- lo anterior a la primera coma y el maximo lo posterior a la ultima
-- Salidas Just TipoHab o Nothing si falta algun campo o el maximo no es un
-- entero mayor que 0
parseTipo :: String -> Maybe TipoHab
parseTipo linea =
  case break (== ',') linea of
    (_, [])           -> Nothing
    (nombre, _ : res) ->
      case break (== ',') (reverse res) of
        (_, []) -> Nothing
        (maxRev, _ : descRev) ->
          let n = trim nombre
          in case parseEntero (trim (reverse maxRev)) of
               Just m | m > 0 && not (null n) ->
                 Just (TipoHab n (trim (reverse descRev)) m)
               _ -> Nothing

-- Objetivo agregar tipos nuevos evitando nombres repetidos sin distinguir
-- mayusculas tanto contra los existentes como dentro de los propios nuevos
-- Entradas tipos existentes y tipos candidatos
-- Salidas pareja con los tipos realmente agregados y la cantidad de repetidos descartados
agregarTipos :: [TipoHab] -> [TipoHab] -> ([TipoHab], Int)
agregarTipos existentes = foldl paso ([], 0)
  where
    clave = map toLower . nombreTipo
    paso (acc, rep) t
      | clave t `elem` map clave (existentes ++ acc) = (acc, rep + 1)
      | otherwise = (acc ++ [t], rep)

-- Objetivo opcion administrativa Cargar tipo de habitaciones
-- Entradas Hotel
-- Salidas Hotel con los tipos nuevos anexados sin cambios si
-- el archivo no se puede leer y los errores se informan sin detener el programa
cargarTipos :: Hotel -> IO Hotel
cargarTipos h = do
  ruta <- leerLinea "Ruta del archivo de tipos de habitacion: "
  r <- leerArchivo ruta
  case r of
    Left err -> putStrLn err >> return h
    Right contenido -> do
      let analizadas = map (\l -> (l, parseTipo l)) (lineasUtiles contenido)
          validos    = [t | (_, Just t) <- analizadas]
          invalidas  = [l | (l, Nothing) <- analizadas]
          (nuevos, repetidos) = agregarTipos (tipos h) validos
      putStrLn ("Tipos agregados: " ++ show (length nuevos)
                ++ " | Repetidos omitidos: " ++ show repetidos
                ++ " | Lineas invalidas: " ++ show (length invalidas))
      mapM_ (putStrLn . ("  Linea invalida omitida: " ++)) invalidas
      return h { tipos = tipos h ++ nuevos }

-- Objetivo crear las habitaciones con ids consecutivos a partir de las
-- cantidades por tipo
-- Entradas lista de pares con el tipo y la cantidad
-- Salidas habitaciones con id desde 1 en adelante
-- agrupadas por tipo en el orden recibido
generarHabitaciones :: [(TipoHab, Int)] -> [Habitacion]
generarHabitaciones pares =
  zipWith (\i t -> Habitacion i (nombreTipo t)) [1 ..]
          (concatMap (\(t, n) -> replicate n t) pares)

-- Objetivo opcion Asignar cantidad de habitaciones por tipo
-- Restricciones solo se puede hacer una vez y requiere tipos cargados y al
-- menos una habitacion en total
-- Entradas Hotel
-- Salidas Hotel con las habitaciones generadas
asignarCantidades :: Hotel -> IO Hotel
asignarCantidades h
  | not (null (habitaciones h)) =
      putStrLn "Las habitaciones ya fueron generadas; no se puede repetir." >> return h
  | null (tipos h) =
      putStrLn "Primero debe cargar los tipos de habitacion." >> return h
  | otherwise = do
      cantidades <- mapM pedir (tipos h)
      let hs = generarHabitaciones (zip (tipos h) cantidades)
      if null hs
        then putStrLn "Debe asignar al menos una habitacion." >> return h
        else putStrLn ("Se generaron " ++ show (length hs) ++ " habitaciones.")
             >> return h { habitaciones = hs }
  where
    pedir t = pedirEntero
      ("Cantidad de habitaciones tipo '" ++ nombreTipo t ++ "': ")
      (const True) "Ingrese un numero entero mayor o igual a 0."

-- Objetivo opcion Ver habitaciones con el id y el nombre del tipo
-- Entradas Hotel
-- Salidas imprime la lista o un aviso si no hay
verHabitaciones :: Hotel -> IO ()
verHabitaciones h
  | null (habitaciones h) = putStrLn "Aun no se han generado habitaciones."
  | otherwise = do
      putStrLn "Id  | Tipo"
      mapM_ (\hb -> putStrLn (pad (show (idHab hb)) ++ " | " ++ tipoHab hb)) (habitaciones h)
  where pad s = s ++ replicate (3 - length s) ' '
  
