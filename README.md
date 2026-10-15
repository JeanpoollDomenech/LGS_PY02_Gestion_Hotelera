# Proyecto Programado #2 - Sistema de Gestión Hotelera (Haskell)

Lenguajes de Programación - TEC - Semestre II, 2026

## Compilar y ejecutar

Desde la carpeta `programa/`:

    ghc -isrc -outputdir build -o hotel src/Main.hs
    ./hotel

Debe ejecutarse desde `programa/` para que la ruta `datos/hotel.txt` se encuentre.
En consolas de Windows con problemas de tildes, ejecutar antes `chcp 65001`.

## Archivos de datos de ejemplo (carpeta `programa/datos/`)

- `hotel.txt`: datos de la empresa gestora.
- `tipos_habitacion.txt`, `tarifas.txt`: cargas válidas de ejemplo.
- `tipos_con_errores.txt`, `tarifas_invalidas.txt`: para probar el manejo de errores.

## Persistencia

El estado se guarda en `programa/datos/estado.dat` (no se sube a git) y se recupera al iniciar.
Para empezar desde cero, borrar ese archivo.
