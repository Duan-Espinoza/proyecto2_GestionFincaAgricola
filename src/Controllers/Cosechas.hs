{-|
Module      : Controllers.Cosechas
Description : Gestión de cosechas en una finca agrícola
Copyright   : (c) 2023
License     : MIT
Maintainer  : duanolivares@estudiantec.cr


Este módulo proporciona funciones para gestionar las cosechas en una finca agrícola. 
Incluye funcionalidades para registrar, consultar, modificar, cerrar y cancelar cosechas, 
así como para verificar la disponibilidad de parcelas y generar informes detallados.

Funciones principales:
- `registrarCosecha`: Registra una nueva cosecha en el sistema.
- `consultarCosecha`: Consulta los detalles de una cosecha específica.
- `cerrarCosecha`: Marca una cosecha como completada y registra la cantidad final recolectada.
- `modificarCosecha`: Permite modificar los datos de una cosecha existente.
- `cancelarCosecha`: Cambia el estado de una cosecha a "Cancelada".
- `verDisponibilidadParcela`: Verifica si una parcela está disponible en un rango de fechas.
- `menuGestionCosechas`: Despliega un menú interactivo para gestionar las cosechas.
- `informeCosechas`: Genera un informe detallado de las cosechas registradas.

Funciones auxiliares:
- `consultarDisponibilidadParcelas`: Consulta la disponibilidad de parcelas en un rango de fechas.
- `consultarEstadoDiarioParcelas`: Muestra el estado diario de las parcelas en un rango de fechas.
- `mostrarDetalleCosecha`: Muestra los detalles de una cosecha específica.
- `actualizarCosecha`: Actualiza los datos de una cosecha en el archivo CSV.
- `pedirDatosModificacion`: Solicita los nuevos valores para modificar una cosecha.
- `validarModificacion`: Valida los cambios realizados a una cosecha.
- `actualizarEstado`: Cambia el estado de una cosecha en el archivo CSV.

Restricciones generales:
- Las fechas deben tener un formato válido (YYYY-MM-DD).
- Los IDs de trabajadores y parcelas deben existir en el sistema.
- Las parcelas deben estar disponibles en el rango de fechas especificado.
- Las cantidades deben ser números positivos.

Este módulo utiliza archivos CSV para almacenar los datos de las cosechas y realiza validaciones 
para garantizar la integridad de los datos. También incluye manejo de errores para operaciones de 
entrada/salida y proporciona estadísticas útiles sobre las cosechas registradas.
-}


-- src/Controllers/Cosechas.hs
{-# LANGUAGE MultiWayIf #-}
module Controllers.Cosechas (
    registrarCosecha,
    consultarCosecha,
    cerrarCosecha,
    modificarCosecha,
    cancelarCosecha,
    verDisponibilidadParcela,
    menuGestionCosechas,
    informeCosechas,
) where

import Models.Cosecha
import Models.Parcela as P (Parcela(vegetal, codigo, nombre))
import Models.Trabajador (trabajadoresIniciales, Trabajador(cedula))
import Controllers.Parcelas (leerParcelas)
import System.IO
import Data.List (find, intercalate, maximumBy, sortOn)
import Data.Ord (comparing)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Time (Day)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import Data.Char (toLower, isSpace)
import Controllers.Herramientas (cargarHerramientasDefault)
import System.IO (withFile, IOMode(..), hPutStrLn)
import Data.Maybe (isNothing)
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Time.Calendar (toGregorian, fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Calendar (addDays, diffDays)


-- Función para normalizar strings (eliminar espacios y convertir a minúsculas)
normalizar :: String -> String
normalizar = map toLower . filter (not . isSpace)

cosechasPath :: FilePath
cosechasPath = "src/data/Cosechas.csv"

-- Nombre: registrarCosecha
-- Entrada: Ninguna entrada directa (interacción por consola)
-- Salida: Efecto secundario de registrar una nueva cosecha en archivo CSV
-- Restricciones: 
--   - El ID del trabajador debe existir en la lista de trabajadores iniciales.
--   - El ID de parcela debe existir y el vegetal debe coincidir con el de la parcela.
--   - Las fechas deben tener formato válido y la parcela debe estar disponible en ese rango.
--   - La cantidad debe ser un número válido (positivo).

registrarCosecha :: IO ()
registrarCosecha = do
    putStrLn "\n--- Registrar Nueva Cosecha ---"
    putStr "ID Trabajador: " >> hFlush stdout
    tid <- getLine
    putStr "ID Parcela: " >> hFlush stdout
    pid <- getLine
    putStr "Fecha Inicio (YYYY-MM-DD): " >> hFlush stdout
    fi <- getLine
    putStr "Fecha Fin (YYYY-MM-DD): " >> hFlush stdout
    ff <- getLine
    putStr "Vegetal: " >> hFlush stdout
    veg <- getLine
    putStr "Cantidad (kg): " >> hFlush stdout
    cantStr <- getLine

    let mtrabajador = find ((== tid) . cedula) trabajadoresIniciales
    herramientasDisponibles <- cargarHerramientasDefault  
    mparcelas <- leerParcelas herramientasDisponibles
    let mparcela = find ((== pid) . codigo) mparcelas

    case (mtrabajador, mparcela) of
        (Just t, Just p) | normalizar (P.vegetal p) == normalizar veg -> do
            case (parsearFecha fi, parsearFecha ff, readMaybe cantStr) of
                (Just fechaI, Just fechaF, Just cant) -> do
                    disponible <- verDisponibilidadParcela pid fechaI fechaF
                    if disponible
                        then do
                            idC <- generarIdCosecha
                            let nuevaCosecha = Cosecha idC tid pid fechaI fechaF veg cant Planificada
                            withFile cosechasPath AppendMode $ \handle -> do
                                hPutStrLn handle (cosechaToCSV nuevaCosecha)
                            putStrLn $ "Cosecha registrada! ID: " ++ idC
                        else putStrLn "Parcela no disponible en esas fechas"
                _ -> putStrLn "Error en fechas o cantidad"
        _ -> putStrLn "Validación fallida: Trabajador/Parcela/Vegetal incorrecto"

-- Nombre: consultarCosecha
-- Entrada: Ninguna entrada directa (solicita el ID de la cosecha por consola)
-- Salida: Muestra en consola los detalles de la cosecha correspondiente
-- Restricciones: El ID debe corresponder a una cosecha existente

consultarCosecha :: IO ()
consultarCosecha = do
    putStr "ID Cosecha: " >> hFlush stdout
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> mostrarDetalleCosecha c
        Nothing -> putStrLn "Cosecha no encontrada"

mostrarDetalleCosecha :: Cosecha -> IO ()
mostrarDetalleCosecha c = do
    putStrLn "\n=== Detalle de Cosecha ==="
    putStrLn $ "ID:            " ++ idCosecha c
    putStrLn $ "Trabajador:    " ++ trabajadorId c
    putStrLn $ "Parcela:       " ++ parcelaId c
    putStrLn $ "Fechas:        " ++ show (fechaInicio c) ++ " a " ++ show (fechaFin c)
    putStrLn $ "Vegetal:       " ++ Models.Cosecha.vegetal c
    putStrLn $ "Cantidad (kg): " ++ show (cantidad c)
    putStrLn $ "Estado:        " ++ show (estado c)
    putStrLn "==========================="

-- Nombre: cerrarCosecha
-- Entrada: Ninguna entrada directa (solicita ID de cosecha y cantidad real por consola)
-- Salida: Actualiza el estado de la cosecha a "Completada" y registra la cantidad final
-- Restricciones: 
--   - La cosecha debe existir.
--   - No debe estar ya cancelada ni completada.
--   - La cantidad ingresada debe ser un número positivo.

cerrarCosecha :: IO ()
cerrarCosecha = do
    putStr "ID Cosecha: " >> hFlush stdout
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> do
            case estado c of
                Completada -> putStrLn "La cosecha ya está cerrada."
                Cancelada -> putStrLn "No se puede cerrar una cosecha cancelada."
                _ -> do
                    putStr "Cantidad recolectada real (kg): " >> hFlush stdout
                    recolectadoStr <- getLine
                    case readMaybe recolectadoStr of
                        Just recolectado | recolectado >= 0 -> do
                            let cActualizada = c {
                                cantidad = recolectado,
                                estado = Completada
                            }
                            actualizarCosecha cActualizada
                            putStrLn "Cosecha cerrada exitosamente."
                        _ -> putStrLn "Cantidad inválida"
        Nothing -> putStrLn "Cosecha no encontrada"

-- Añadir nueva función de actualización completa
actualizarCosecha :: Cosecha -> IO ()
actualizarCosecha cActualizada = do
    cosechas <- leerCosechas
    let actualizadas = map (\c -> if idCosecha c == idCosecha cActualizada then cActualizada else c) cosechas
    withFile cosechasPath WriteMode $ \handle -> 
        hPutStr handle (unlines (map cosechaToCSV actualizadas))

-- Nombre: modificarCosecha
-- Entrada: Ninguna directa (se pide por consola el ID de la cosecha y los nuevos valores)
-- Salida: Actualiza los datos de una cosecha en el archivo CSV
-- Restricciones:
--   - La cosecha debe existir y no estar en estado "Completada".
--   - La parcela nueva debe existir y estar disponible en el nuevo rango de fechas.

modificarCosecha :: IO ()
modificarCosecha = do
    putStr "ID Cosecha a modificar: " >> hFlush stdout
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> do
            case estado c of
                Completada -> putStrLn "No se puede modificar una cosecha cerrada."
                _ -> do
                    -- Obtener nuevos datos
                    (nuevaPid, nuevasFechas, nuevoVegetal) <- pedirDatosModificacion c
                    
                    -- Validar cambios
                    resultadoValidacion <- validarModificacion c nuevaPid nuevasFechas nuevoVegetal
                    
                    case resultadoValidacion of
                        Right cActualizada -> do
                            actualizarCosecha cActualizada
                            putStrLn "Modificación exitosa."
                        Left mensajeError -> putStrLn mensajeError
        Nothing -> putStrLn "Cosecha no encontrada"

-- Función auxiliar para pedir datos de modificación
pedirDatosModificacion :: Cosecha -> IO (String, (Maybe Day, Maybe Day), String)
pedirDatosModificacion c = do
    putStrLn "\nDeje en blanco para mantener el valor actual"
    
    -- Obtener datos actualizados de parcelas
    herramientas <- cargarHerramientasDefault
    parcelas <- leerParcelas herramientas
    
    -- Nueva parcela
    putStr $ "Nueva Parcela [" ++ parcelaId c ++ "]: "
    hFlush stdout
    nuevaPidRaw <- getLine
    let nuevaPid = if null nuevaPidRaw then parcelaId c else nuevaPidRaw
    
    -- Obtener vegetal por defecto según parcela
    let mParcela = find ((== nuevaPid) . codigo) parcelas
    let vegetalPorDefecto = maybe (Models.Cosecha.vegetal c) P.vegetal mParcela
    
    -- Nuevas fechas
    putStr $ "Nueva Fecha Inicio [" ++ show (fechaInicio c) ++ "]: "
    hFlush stdout
    nuevaFiStr <- getLine
    putStr $ "Nueva Fecha Fin [" ++ show (fechaFin c) ++ "]: "
    hFlush stdout
    nuevaFfStr <- getLine
    
    -- Mostrar vegetal por defecto y pedir modificación
    putStr $ "Nuevo Vegetal [Auto: " ++ vegetalPorDefecto ++ "]: "
    hFlush stdout
    nuevoVegetalRaw <- getLine
    let nuevoVegetal = if null nuevoVegetalRaw then vegetalPorDefecto else nuevoVegetalRaw
    
    let nuevasFechas = (parsearFecha nuevaFiStr, parsearFecha nuevaFfStr)
    
    return (nuevaPid, nuevasFechas, nuevoVegetal)

-- Función de validación de modificación (versión simplificada)
validarModificacion :: Cosecha -> String -> (Maybe Day, Maybe Day) -> String -> IO (Either String Cosecha)
validarModificacion c nuevaPid (nuevaFi, nuevaFf) _ = do  -- Eliminamos nuevoVegetal de los parámetros
    -- Validar parcelas
    herramientas <- cargarHerramientasDefault
    parcelas <- leerParcelas herramientas
    let mNuevaParcela = find ((== nuevaPid) . codigo) parcelas
    
    -- Obtener vegetal de la nueva parcela
    let vegetalParcela = maybe "" P.vegetal mNuevaParcela
    
    -- Validar fechas
    let fi = maybe (fechaInicio c) id nuevaFi
    let ff = maybe (fechaFin c) id nuevaFf
    disponible <- verDisponibilidadParcela nuevaPid fi ff
    
    -- Construir cosecha actualizada con vegetal de la parcela
    let cActualizada = c {
        parcelaId = nuevaPid,
        fechaInicio = fi,
        fechaFin = ff,
        Models.Cosecha.vegetal = vegetalParcela  -- Usamos siempre el vegetal de la parcela
    }
    
    -- Chequear validaciones
    if | isNothing mNuevaParcela -> return $ Left "Parcela no válida"
       | not disponible -> return $ Left "Parcela no disponible en esas fechas"
       | otherwise -> return $ Right cActualizada

-- Nombre: cancelarCosecha
-- Entrada: Ninguna directa (solicita por consola el ID de la cosecha)
-- Salida: Actualiza el estado de la cosecha a "Cancelada"
-- Restricciones:
--   - La cosecha debe existir.
--   - No debe estar en estado "Completada" ni "Cancelada".

cancelarCosecha :: IO ()
cancelarCosecha = do
    putStr "ID Cosecha: " >> hFlush stdout
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> do
            case estado c of
                Completada -> putStrLn "No se puede cancelar una cosecha cerrada."
                Cancelada -> putStrLn "La cosecha ya está cancelada."
                _ -> do
                    actualizarEstado c Cancelada
                    putStrLn "Cosecha cancelada exitosamente."
        Nothing -> putStrLn "Cosecha no encontrada"

-- Nombre: verDisponibilidadParcela
-- Entrada: 
--   - pid: ID de la parcela (String)
--   - fi: Fecha de inicio (Day)
--   - ff: Fecha de fin (Day)
-- Salida: Booleano indicando si la parcela está disponible
-- Restricciones: Las fechas deben ser válidas. Se considera ocupada si hay solapamiento con otra cosecha.

verDisponibilidadParcela :: String -> Day -> Day -> IO Bool
verDisponibilidadParcela pid fi ff = do
    cosechas <- leerCosechas
    return $ not (any (\c -> parcelaId c == pid && solapamientoFechas c (Cosecha "" "" "" fi ff "" 0 Planificada)) cosechas)

-- Funciones auxiliares
-- uso withFile
-- Lectura de CSV a Cosecha
leerCosechas :: IO [Cosecha]
leerCosechas = handleIOError $ do
    exists <- doesFileExist cosechasPath
    if not exists 
        then return []
        else withFile cosechasPath ReadMode $ \handle -> do
            contenido <- hGetContents handle
            length contenido `seq` return (mapMaybe csvToCosecha (lines contenido))

handleIOError :: IO [a] -> IO [a]
handleIOError action = catch action handler
    where
        handler :: IOException -> IO [a]
        handler e = do
            putStrLn $ "Error accediendo al archivo de cosechas: " ++ show e
            return []

generarIdCosecha :: IO String
generarIdCosecha = do
    cosechas <- leerCosechas  -- Ahora usa la versión segura de leerCosechas
    let lastId = if null cosechas then 0 else read (drop 4 (idCosecha (last cosechas))) :: Int
    return $ "COS-" ++ show (lastId + 1)

-- Modificar actualizarEstado para usar withFile
actualizarEstado :: Cosecha -> EstadoCosecha -> IO ()
actualizarEstado c nuevoEstado = do
    cosechas <- leerCosechas
    let updated = map (\x -> if idCosecha x == idCosecha c then x { estado = nuevoEstado } else x) cosechas
    withFile cosechasPath WriteMode $ \handle -> 
        hPutStr handle (unlines (map cosechaToCSV updated))


-- src/Controllers/Cosechas.hs

-- Nueva función para consulta de disponibilidad
consultarDisponibilidadParcelas :: IO ()
consultarDisponibilidadParcelas = do
    putStrLn "\n--- Consulta de Disponibilidad de Parcelas ---"
    putStr "Fecha inicio (YYYY-MM-DD): " >> hFlush stdout
    fiStr <- getLine
    putStr "Fecha fin (YYYY-MM-DD): " >> hFlush stdout
    ffStr <- getLine
    
    case (parsearFecha fiStr, parsearFecha ffStr) of
        (Just fi, Just ff) -> do
            cosechas <- leerCosechas
            herramientas <- cargarHerramientasDefault
            parcelas <- leerParcelas herramientas
            
            let disponibles = filter (\p -> 
                    not (any (\c -> 
                        parcelaId c == codigo p && 
                        solapamientoFechas c (Cosecha "" "" "" fi ff "" 0 Planificada)
                    ) cosechas)) parcelas
                
            putStrLn "\nParcelas disponibles en el rango:"
            mapM_ (\p -> putStrLn $ "- " ++ codigo p ++ " (" ++ P.vegetal p ++ ")") disponibles
            
        _ -> putStrLn "Error en el formato de fechas"

-- Función para estado diario por parcela
consultarEstadoDiarioParcelas :: IO ()
consultarEstadoDiarioParcelas = do
    putStrLn "\n--- Estado Diario de Parcelas ---"
    putStr "Fecha inicio (YYYY-MM-DD): " >> hFlush stdout
    fiStr <- getLine
    putStr "Fecha fin (YYYY-MM-DD): " >> hFlush stdout
    ffStr <- getLine
    
    case (parsearFecha fiStr, parsearFecha ffStr) of
        (Just fi, Just ff) -> do
            cosechas <- leerCosechas
            herramientas <- cargarHerramientasDefault
            parcelas <- leerParcelas herramientas
            
            let dias = [fi .. ff]
            
            putStrLn "\nEstado de parcelas por día:"
            forM_ parcelas $ \p -> do
                putStrLn $ "\nParcela " ++ codigo p ++ " (" ++ P.vegetal p ++ "):"
                forM_ dias $ \dia -> do
                    let ocupada = any (\c -> parcelaId c == codigo p && dia >= fechaInicio c && dia <= fechaFin c) cosechas
                    putStrLn $ "  " ++ show dia ++ ": " ++ if ocupada then "OCUPADA" else "Disponible"
                    
        _ -> putStrLn "Error en el formato de fechas"


-- Nombre: informeCosechas
-- Entrada: Ninguna directa (interacción por consola)
-- Salida: Genera un informe de las cosechas registradas

informeCosechas :: IO ()
informeCosechas = do
    putStrLn "\n--- Informe de Cosechas ---"
    cosechas <- leerCosechas
    herramientas <- cargarHerramientasDefault
    parcelas <- leerParcelas herramientas
    
    -- Mostrar información de todas las cosechas
    putStrLn "\nInformación de Cosechas:"
    forM_ cosechas $ \c -> do
        let mParcela = find ((== parcelaId c) . codigo) parcelas
        let parcelaNombre = maybe "Desconocida" P.nombre mParcela
        putStrLn $ "- ID: " ++ idCosecha c ++ ", Parcela: " ++ parcelaNombre ++ ", Vegetal: " ++ Models.Cosecha.vegetal c ++ ", Fecha: " ++ show (fechaInicio c) ++ " a " ++ show (fechaFin c) ++ ", Cantidad: " ++ show (cantidad c) ++ " kg, Trabajador: " ++ trabajadorId c
    
    -- Aquí se pueden agregar las estadísticas solicitadas
    let parcelaMayorVolumen = maximumBy (comparing cantidad) cosechas
    putStrLn $ "\nParcela con mayor volumen de cosecha: " ++ idCosecha parcelaMayorVolumen ++ " con " ++ show (cantidad parcelaMayorVolumen) ++ " kg"
    

    let top3Parcelas = take 3 $ reverse $ sortOn cantidad cosechas
    putStrLn "\nTop 3 de parcelas con mayor venta:"
    mapM_ (\c -> putStrLn $ "- ID: " ++ idCosecha c ++ ", Cantidad: " ++ show (cantidad c) ++ " kg") top3Parcelas

    let trabajadorConMasCosechas = maximumBy (comparing (\tid -> length (filter ((== tid) . trabajadorId) cosechas))) (map trabajadorId cosechas)
    putStrLn $ "\nTrabajador con más cosechas realizadas: " ++ trabajadorConMasCosechas

    
    -- Muestra el mes-año con mayor recolección acumulada
    let recoleccionesPorMes = foldl (\acc c -> 
            let (año, mes, _) = toGregorian (fechaInicio c)
                mesAño = show mes ++ "-" ++ show año
            in Map.insertWith (+) mesAño (cantidad c) acc) Map.empty cosechas
    let (mesAñoMax, maxCantidad) = maximumBy (comparing snd) (Map.toList recoleccionesPorMes)
    putStrLn $ "\nMes y Año con mayor recolección acumulada: " ++ mesAñoMax ++ " con " ++ show maxCantidad ++ " kg"

    
    --Muestra las cosechas con subproducción y sobreproducción
    let subproduccion = filter (\c -> cantidad c < 0) cosechas
    let sobreproduccion = filter (\c -> cantidad c > 0) cosechas
    putStrLn "\nCosechas con subproducción:"
    mapM_ (\c -> putStrLn $ "- ID: " ++ idCosecha c ++ ", Cantidad: " ++ show (cantidad c) ++ " kg") subproduccion
    putStrLn "\nCosechas con sobreproducción:"
    mapM_ (\c -> putStrLn $ "- ID: " ++ idCosecha c ++ ", Cantidad: " ++ show (cantidad c) ++ " kg") sobreproduccion
    putStrLn "\nInforme generado exitosamente."

-- Nombre: menuGestionCosechas
-- Entrada: Ninguna
-- Salida: Despliega menú interactivo en consola para acceder a las funcionalidades de gestión de cosechas
-- Restricciones: La entrada debe ser un número entre 1 y 8; maneja errores de entrada mediante recursión

menuGestionCosechas :: IO ()
menuGestionCosechas = do
    putStrLn "\n--- Menú de Gestión de Cosechas ---"
    putStrLn "1. Registrar Cosecha"
    putStrLn "2. Consultar Cosecha"
    putStrLn "3. Cerrar Cosecha"
    putStrLn "4. Modificar Cosecha"
    putStrLn "5. Cancelar Cosecha"
    putStrLn "6. Consultar disponibilidad parcela por rango"
    putStrLn "7. Estado diario de parcelas"
    putStrLn "8. Volver al Menú Principal"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> registrarCosecha >> menuGestionCosechas
        "2" -> consultarCosecha >> menuGestionCosechas
        "3" -> cerrarCosecha >> menuGestionCosechas
        "4" -> modificarCosecha >> menuGestionCosechas
        "5" -> cancelarCosecha >> menuGestionCosechas
        "6" -> consultarDisponibilidadParcelas >> menuGestionCosechas
        "7" -> consultarEstadoDiarioParcelas >> menuGestionCosechas
        "8" -> putStrLn "Volviendo al menú principal..." >> return ()
        _   -> putStrLn "Opción inválida, intente nuevamente." >> menuGestionCosechas