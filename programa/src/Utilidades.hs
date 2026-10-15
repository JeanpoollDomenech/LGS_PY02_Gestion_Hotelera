-- Modulo Utilidades
-- Objetivo funciones de apoyo usadas por todo el sistema lectura segura de
-- entradas del usuario validacion de fechas lectura de archivos y formato
module Utilidades
  ( trim, splitarPor, parseEntero, parseFecha, parseMonto
  , leerLinea, leerOpcion, pedirEntero, pedirFecha, pedirTexto
  , leerArchivo, lineasUtiles, formatoMonto, nombreDia
  ) where

import Control.Exception (IOException, evaluate, try)
import Data.Char (isDigit, isSpace)
import Data.Time.Calendar (Day, DayOfWeek (..), fromGregorianValid)
import System.Exit (exitSuccess)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Objetivo quitar espacios al inicio y al final de un texto
-- Entradas String
-- Salidas String sin espacios en los extremos
trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

-- Objetivo dividir un texto en partes usando un separador
-- Entradas caracter separador y texto
-- Salidas lista de partes nunca vacia
splitarPor :: Char -> String -> [String]
splitarPor c s = case break (== c) s of
  (a, [])      -> [a]
  (a, _ : res) -> a : splitarPor c res

-- Objetivo convertir un texto en entero no negativo
-- Entradas String
-- Salidas Just n si son solo digitos de 1 a 9 y Nothing si no
parseEntero :: String -> Maybe Int
parseEntero s
  | not (null s) && length s <= 9 && all isDigit s = readMaybe s
  | otherwise = Nothing

-- Objetivo convertir un texto en un monto valido
-- Entradas String
-- Salidas Just monto si es numero finito y mayor o igual a 0
parseMonto :: String -> Maybe Double
parseMonto s = case readMaybe s of
  Just m | not (isNaN m || isInfinite m) && m >= 0 -> Just m
  _ -> Nothing

-- Objetivo convertir un texto AAAA-MM-DD en una fecha real del calendario
-- Entradas String
-- Salidas Just Day si la fecha existe y rechaza el 30 de febrero
-- Nothing en cualquier otro caso
parseFecha :: String -> Maybe Day
parseFecha s = case splitarPor '-' (trim s) of
  [a, b, c] | length a == 4 -> do
    anio <- parseEntero a
    mes  <- parseEntero b
    dia  <- parseEntero c
    fromGregorianValid (toInteger anio) mes dia
  _ -> Nothing

-- Objetivo mostrar un mensaje y leer una linea del usuario sin espacios
-- extremos y si la entrada termina por fin de archivo cierra el programa
-- Entradas mensaje
-- Salidas linea leida
leerLinea :: String -> IO String
leerLinea msg = do
  putStr msg
  hFlush stdout
  fin <- isEOF
  if fin
    then putStrLn "\nFin de la entrada. Saliendo..." >> exitSuccess
    else trim <$> getLine

-- Objetivo leer una opcion de menu
-- Entradas mensaje
-- Salidas Just n si es entero valido Nothing si no
leerOpcion :: String -> IO (Maybe Int)
leerOpcion msg = parseEntero <$> leerLinea msg

-- Objetivo pedir un entero hasta que cumpla una condicion
-- Entradas mensaje condicion de validez y mensaje de error
-- Salidas entero valido repite la pregunta mientras sea invalido
pedirEntero :: String -> (Int -> Bool) -> String -> IO Int
pedirEntero msg valido err = do
  s <- leerLinea msg
  case parseEntero s of
    Just n | valido n -> return n
    _ -> putStrLn err >> pedirEntero msg valido err

-- Objetivo pedir una fecha hasta que sea valida
-- Entradas mensaje
-- Salidas Day valido
pedirFecha :: String -> IO Day
pedirFecha msg = do
  s <- leerLinea msg
  case parseFecha s of
    Just d  -> return d
    Nothing -> putStrLn "Fecha invalida. Use el formato AAAA-MM-DD (ej. 2026-12-24)."
               >> pedirFecha msg

-- Objetivo pedir un texto que no este vacio
-- Entradas mensaje
-- Salidas texto no vacio
pedirTexto :: String -> IO String
pedirTexto msg = do
  s <- leerLinea msg
  if null s then putStrLn "El valor no puede estar vacio." >> pedirTexto msg
            else return s

-- Objetivo leer un archivo de texto completo UTF-8 sin dejar el archivo
-- abierto controlando errores de ruta o permisos
-- Entradas ruta
-- Salidas Right contenido o Left mensaje de error
leerArchivo :: FilePath -> IO (Either String String)
leerArchivo ruta = do
  r <- try leer :: IO (Either IOException String)
  return (either (const (Left ("No se pudo leer el archivo: " ++ ruta))) Right r)
  where
    leer = do
      h <- openFile ruta ReadMode
      hSetEncoding h utf8
      c <- hGetContents h
      _ <- evaluate (length c)
      hClose h
      return (dropWhile (== '\xFEFF') c)   -- elimina el BOM si existe

-- Objetivo separar un contenido en lineas no vacias y sin espacios extremos
-- Entradas contenido del archivo
-- Salidas lista de lineas utiles
lineasUtiles :: String -> [String]
lineasUtiles = filter (not . null) . map trim . lines

-- Objetivo dar formato a un monto con dos decimales
-- Entradas Double
-- Salidas String con dos decimales
formatoMonto :: Double -> String
formatoMonto = printf "%.2f"

-- Objetivo nombre en espanol de un dia de la semana
-- Entradas DayOfWeek
-- Salidas String
nombreDia :: DayOfWeek -> String
nombreDia Monday    = "Lunes"
nombreDia Tuesday   = "Martes"
nombreDia Wednesday = "Miercoles"
nombreDia Thursday  = "Jueves"
nombreDia Friday    = "Viernes"
nombreDia Saturday  = "Sabado"
nombreDia Sunday    = "Domingo"
