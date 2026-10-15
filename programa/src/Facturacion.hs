-- Modulo Facturacion
-- Objetivo emitir la factura de una reservacion activa y marcarla Facturada
module Facturacion (facturarReserva) where

import Data.List (find)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Reservas
import Tipos
import Utilidades

-- Objetivo opcion Facturar reservacion
-- Restricciones la reservacion debe existir y estar Activa
-- Calculo el subtotal es el monto de la reserva el impuesto es el 13% del subtotal
-- y el total es subtotal mas impuesto
-- Entradas Hotel
-- Salidas Hotel con la factura registrada y la reserva en
-- estado Facturada y el contador de facturas incrementado o sin cambios
facturarReserva :: Hotel -> IO Hotel
facturarReserva h = do
  i <- pedirEntero "Numero de reservacion a facturar: " (const True) "Ingrese un numero entero."
  case find ((== i) . idRes) (reservas h) of
    Nothing -> putStrLn "No existe una reservacion con ese codigo." >> return h
    Just r
      | estadoRes r /= Activa ->
          putStrLn ("No se puede facturar: la reservacion esta " ++ show (estadoRes r) ++ ".") >> return h
      | otherwise -> do
          ahora <- zonedTimeToLocalTime <$> getZonedTime
          let sub = montoRes r
              imp = sub * tasaIVA
              f = Factura { idFac = sigFac h, idResFac = i, fechaHoraFac = ahora
                          , subtotalFac = sub, impuestoFac = imp, totalFac = sub + imp }
          putStrLn (formatoFactura (info h) f r)
          return h { facturas = facturas h ++ [f]
                   , reservas = map (\x -> if idRes x == i then x { estadoRes = Facturada } else x)
                                    (reservas h)
                   , sigFac = sigFac h + 1 }

-- Objetivo construir el texto de la factura
-- Entradas informacion del local factura y reserva facturada
-- Salidas String de varias lineas listo para imprimir
formatoFactura :: InfoHotel -> Factura -> Reserva -> String
formatoFactura inf f r = unlines
  [ "\n=============== FACTURA ==============="
  , "Factura #" ++ show (idFac f) ++ " | Reservacion #" ++ show (idResFac f)
  , "Fecha y hora: " ++ formatoHora (fechaHoraFac f)
  , "--- Local ---"
  , ihNombre inf
  , "Cedula juridica: " ++ ihCedula inf
  , "Sitio web: " ++ ihSitioWeb inf ++ " | Telefono: " ++ ihTelefono inf
  , ihProvincia inf ++ ", " ++ ihPais inf
  , "--- Detalle de la reservacion ---"
  , formatoReserva r { estadoRes = Facturada }
  , "--- Totales ---"
  , "Subtotal:                  " ++ formatoMonto (subtotalFac f)
  , "Impuesto de ventas (13%):  " ++ formatoMonto (impuestoFac f)
  , "TOTAL:                     " ++ formatoMonto (totalFac f)
  , "========================================" ]
