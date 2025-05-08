{-|
Module      : Controllers.Menu
Description : Módulo para gestionar el menú principal y la interacción del usuario con el sistema.
Copyright   : (c) 2025
License     : MIT
Maintainer  : geovanniga32@gmail.com
Stability   : experimental
Portability : portable

Este módulo contiene las funciones necesarias para iniciar el sistema, autenticar a los trabajadores mediante su cédula,
mostrar las opciones del menú principal, y dirigir a submenús operativos o generales según el rol del trabajador.

== Funcionalidades

* Inicio del sistema y presentación del menú principal.
* Validación de acceso mediante la cédula de trabajadores.
* Menús operativos para gestión de herramientas, parcelas y reportes.
* Opciones generales informativas.

Este controlador actúa como el punto de entrada principal al sistema de gestión de finca agrícola.
-}

module Controllers.Menu (mostrarMenuInicio) where

-- Importaciones
import System.IO ( hFlush, stdout )
import Models.Trabajador (Trabajador, trabajadoresIniciales)
import Controllers.Trabajadores (validarAcceso, mostrarInformacionTrabajador)
import Controllers.Herramientas (cargarHerramientasDesdeArchivo)
import Controllers.Parcelas (registrarParcela, consultarParcela, leerParcelas, csvToParcela)
import Models.Herramienta (Herramienta)
import Controllers.Cosechas (menuGestionCosechas, informeCosechas)


-- | Función principal que muestra el título del sistema y lanza el menú principal.
mostrarMenuInicio :: IO ()
mostrarMenuInicio = do
    putStrLn "\n======================================="
    putStrLn "   Sistema de Gestión de Finca Agrícola"
    putStrLn "======================================="
    menuPrincipal

-- | Muestra el menú principal y gestiona la navegación inicial del sistema.
--
-- Ofrece las siguientes opciones:
--
-- 1. Acceso a opciones operativas (requiere autenticación)
-- 2. Acceso a opciones generales
-- 3. Salir del sistema
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n--- Menú Principal ---"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> autenticarTrabajador
        "2" -> opcionesGenerales
        "3" -> putStrLn "\nGracias por utilizar el sistema. ¡Hasta luego!"
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            menuPrincipal

-- | Solicita la cédula del trabajador y valida su acceso.
--
-- Si la cédula es válida, se muestra su información y se accede al menú operativo.
-- En caso contrario, permite reintentar la autenticación.
autenticarTrabajador :: IO ()
autenticarTrabajador = do
    putStr   "\nIngrese su cédula: "
    hFlush stdout
    ced <- getLine
    trabajador <- validarAcceso ced trabajadoresIniciales
    case trabajador of
        Just t -> do
            mostrarInformacionTrabajador t
            menuOperativo t []
        Nothing -> do
            putStrLn "\n Cédula no registrada. Intente de nuevo."
            autenticarTrabajador

-- | Muestra el menú operativo para trabajadores autenticados.
--
-- Ofrece opciones como:
--
-- 1. Cargar y mostrar herramientas desde archivo
-- 2. Registrar y mostrar parcelas de cultivo 
-- 3. Ver informe de cosechas (por implementar)
-- 4. Volver al menú principal
--
-- El menú se actualiza dinámicamente según las herramientas cargadas.
menuOperativo :: Trabajador -> [Herramienta] -> IO ()
menuOperativo t herramientas = do
    putStrLn "\n--- Opciones Operativas ---"
    putStrLn "1. Cargar y Mostrar Herramientas de Campo"
    putStrLn "2. Registrar y Mostrar Parcelas de Cultivo"
    putStrLn "3. Informe de Cosechas"
    putStrLn "4. Volver al Menú Principal"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            nuevas <- cargarHerramientasDesdeArchivo []
            let nuevasDetectadas = filter (`notElem` herramientas) nuevas
            if null nuevasDetectadas
                then do
                    putStrLn "\n No se detectaron nuevas herramientas."
                    menuOperativo t herramientas
                else do
                    putStrLn "\n--- Nuevas herramientas agregadas ---"
                    mapM_ print nuevasDetectadas
                    let herramientasActualizadas = herramientas ++ nuevasDetectadas
                    menuOperativo t herramientasActualizadas

        "2" -> do
            putStrLn "Menu de Parcelas"
            putStrLn "1. Consultar Parcela"
            putStrLn "2. Registrar Parcela"
            putStr   "Seleccione una opción: "
            hFlush stdout
            opcionParcela <- getLine
            case opcionParcela of
                "1" -> do
                    parcelas <- leerParcelas herramientas
                    consultarParcela parcelas
                    menuOperativo t herramientas
                "2" -> do
                    parcelas <- registrarParcela [] herramientas
                    menuOperativo t herramientas
        "3" -> do
            informeCosechas
            menuOperativo t herramientas
        "4" -> menuPrincipal
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            menuOperativo t herramientas


-- | Muestra el submenú de opciones generales.
--
-- Incluye funcionalidades informativas como estadísticas (por implementar) e información general de la finca.
-- | Muestra el submenú de opciones generales.
opcionesGenerales :: IO ()
opcionesGenerales = do
    putStrLn "\n--- Opciones Generales ---"
    putStrLn "1. Gestión de Cosechas"
    putStrLn "2. Volver al Menú Principal"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            menuGestionCosechas
            opcionesGenerales

        "2" -> menuPrincipal
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            opcionesGenerales

