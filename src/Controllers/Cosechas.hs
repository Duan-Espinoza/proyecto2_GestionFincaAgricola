-- src/Controllers/Cosechas.hs
module Controllers.Cosechas (
    registrarCosecha,
    consultarCosecha,
    cerrarCosecha,
    modificarCosecha,
    cancelarCosecha,
    verDisponibilidadParcela,
    menuGestionCosechas
) where

import Models.Cosecha
import Models.Parcela as P (Parcela(vegetal, codigo))
import Models.Trabajador (trabajadoresIniciales, Trabajador(cedula))
import Controllers.Parcelas (leerParcelas)
import System.IO
import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Time (Day)
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)

cosechasPath :: FilePath
cosechasPath = "src/data/Cosechas.csv"

registrarCosecha :: IO ()
registrarCosecha = do
    putStrLn "\n--- Registrar Nueva Cosecha ---"
    putStr "ID Trabajador: "
    tid <- getLine
    putStr "ID Parcela: "
    pid <- getLine
    putStr "Fecha Inicio (YYYY-MM-DD): "
    fi <- getLine
    putStr "Fecha Fin (YYYY-MM-DD): "
    ff <- getLine
    putStr "Vegetal: "
    veg <- getLine
    putStr "Cantidad (kg): "
    cantStr <- getLine

    let mtrabajador = find ((== tid) . cedula) trabajadoresIniciales
    mparcelas <- leerParcelas []
    let mparcela = find ((== pid) . codigo) mparcelas

    case (mtrabajador, mparcela) of
        (Just t, Just p) | P.vegetal p == veg -> do
            case (parsearFecha fi, parsearFecha ff, readMaybe cantStr) of
                (Just fechaI, Just fechaF, Just cant) -> do
                    disponible <- verDisponibilidadParcela pid fechaI fechaF
                    if disponible
                        then do
                            idC <- generarIdCosecha
                            let nuevaCosecha = Cosecha idC tid pid fechaI fechaF veg cant Planificada
                            appendFile cosechasPath (cosechaToCSV nuevaCosecha ++ "\n")
                            putStrLn $ "Cosecha registrada! ID: " ++ idC
                        else putStrLn "Parcela no disponible en esas fechas"
                _ -> putStrLn "Error en fechas o cantidad"
        _ -> putStrLn "Validación fallida: Trabajador/Parcela/Vegetal incorrecto"

consultarCosecha :: IO ()
consultarCosecha = do
    putStr "ID Cosecha: "
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> putStrLn $ show c
        Nothing -> putStrLn "Cosecha no encontrada"

cerrarCosecha :: IO ()
cerrarCosecha = do
    putStr "ID Cosecha: "
    idC <- getLine
    cosechas <- leerCosechas
    let mc = find ((== idC) . idCosecha) cosechas
    case mc of
        Just c -> actualizarEstado c Completada
        Nothing -> putStrLn "Cosecha no encontrada"


modificarCosecha :: IO ()
modificarCosecha = do
    putStr "ID Cosecha: "
    idC <- getLine
    cosechas <- leerCosechas
    case find ((== idC) . idCosecha) cosechas of
        Just c -> do
            putStrLn "Funcionalidad de modificación pendiente."
        Nothing -> putStrLn "Cosecha no encontrada"

cancelarCosecha :: IO ()
cancelarCosecha = do
    putStr "ID Cosecha: "
    idC <- getLine
    cosechas <- leerCosechas
    let mc = find ((== idC) . idCosecha) cosechas
    case mc of
        Just c -> actualizarEstado c Cancelada
        Nothing -> putStrLn "Cosecha no encontrada"

verDisponibilidadParcela :: String -> Day -> Day -> IO Bool
verDisponibilidadParcela pid fi ff = do
    cosechas <- leerCosechas
    return $ not (any (\c -> parcelaId c == pid && solapamientoFechas c (Cosecha "" "" "" fi ff "" 0 Planificada)) cosechas)

-- Funciones auxiliares
leerCosechas :: IO [Cosecha]
leerCosechas = do
    exists <- doesFileExist cosechasPath
    if not exists 
        then return []
        else readFile cosechasPath >>= return . mapMaybe csvToCosecha . lines

generarIdCosecha :: IO String
generarIdCosecha = do
    cosechas <- leerCosechas
    let lastId = if null cosechas then 0 else read (drop 4 (idCosecha (last cosechas))) :: Int
    return $ "COS-" ++ show (lastId + 1)

actualizarEstado :: Cosecha -> EstadoCosecha -> IO ()
actualizarEstado c nuevoEstado = do
    cosechas <- leerCosechas
    let updated = map (\x -> if idCosecha x == idCosecha c then x { estado = nuevoEstado } else x) cosechas
    writeFile cosechasPath (unlines (map cosechaToCSV updated))


-- | Función para mostrar el menú de gestión de cosechas
menuGestionCosechas :: IO ()
menuGestionCosechas = do
    putStrLn "\n--- Menú de Gestión de Cosechas ---"
    putStrLn "1. Registrar Cosecha"
    putStrLn "2. Consultar Cosecha"
    putStrLn "3. Cerrar Cosecha"
    putStrLn "4. Modificar Cosecha"
    putStrLn "5. Cancelar Cosecha"
    putStrLn "6. Salir"
    putStr "Seleccione una opción: "
    opcion <- getLine
    case opcion of
        "1" -> registrarCosecha
        "2" -> consultarCosecha
        "3" -> cerrarCosecha
        "4" -> modificarCosecha
        "5" -> cancelarCosecha
        "6" -> putStrLn "Saliendo del menú..."
        _   -> putStrLn "Opción inválida, intente nuevamente." >> menuGestionCosechas