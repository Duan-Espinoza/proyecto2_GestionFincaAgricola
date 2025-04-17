-- Archivo: Controllers/Menu.hs
--  Módulo Controllers.Menu
--  Funciones para mostrar el menú principal y gestionar la interacción con el usuario
module Controllers.Menu (mostrarMenuInicio) where

-- Importaciones 
import System.IO ( hFlush, stdout )
import Models.Trabajador (Trabajador, trabajadoresIniciales)
import Controllers.Trabajadores (validarAcceso, mostrarInformacionTrabajador)
import System.IO (hFlush, stdout)


-- Función principal para iniciar el sistema
mostrarMenuInicio :: IO ()
mostrarMenuInicio = do
    putStrLn "\n======================================="
    putStrLn "   Sistema de Gestión de Finca Agrícola"
    putStrLn "======================================="
    menuPrincipal

-- Menú Principal
menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\n--- Menú Principal ---"
    putStrLn "1. Opciones Operativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> autenticarTrabajador
        "2" -> opcionesGenerales
        "3" -> putStrLn "\nGracias por utilizar el sistema. ¡Hasta luego!"
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            menuPrincipal

-- Autenticación del trabajador por cédula
autenticarTrabajador :: IO ()
autenticarTrabajador = do
    putStr   "\nIngrese su cédula: "
    hFlush stdout
    ced <- getLine
    trabajador <- validarAcceso ced trabajadoresIniciales
    case trabajador of
        Just t -> do
            mostrarInformacionTrabajador t
            menuOperativo t
        Nothing -> do
            hFlush stdout
            putStrLn "\n Cédula no registrada. Intente de nuevo."
            autenticarTrabajador

-- Menú Operativo (funciones a implementar)
menuOperativo :: Trabajador -> IO ()
menuOperativo t = do
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
            putStrLn "\n(Función herramientas aún no implementada)"
            menuOperativo t
        "2" -> do
            putStrLn "\n (Función parcelas aún no implementada)"
            menuOperativo t
        "3" -> do
            putStrLn "\n (Informe de cosechas aún no implementado)"
            menuOperativo t
        "4" -> menuPrincipal
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            menuOperativo t

-- Opciones Generales
opcionesGenerales :: IO ()
opcionesGenerales = do
    putStrLn "\n--- Opciones Generales ---"
    putStrLn "1. Ver estadísticas generales"
    putStrLn "2. Ver información de la finca"
    putStrLn "3. Volver al Menú Principal"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\n (Función estadísticas aún no implementada)"
            opcionesGenerales
        "2" -> do
            putStrLn "\n (Función información de finca aún no implementada)"
            opcionesGenerales
        "3" -> menuPrincipal
        _   -> do
            putStrLn "\n Opción inválida. Intente de nuevo."
            opcionesGenerales
