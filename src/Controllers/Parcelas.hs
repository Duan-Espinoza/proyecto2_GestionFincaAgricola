module Controllers.Parcelas (
    registrarParcela,
    consultarParcela,
    leerParcelas,
    csvToParcela
) where

import Models.Parcela (Parcela(..))
import Models.Herramienta (Herramienta)
import qualified Models.Herramienta as H
import System.IO
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Controllers.Herramientas (cargarHerramientasDesdeArchivo, mostrarHerramientas)
import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified Control.Exception as E



-- Ruta del archivo CSV
filePath :: FilePath
filePath  = "C:\\Users\\geova\\Github Desktop Repos\\proyecto2_GestionFincaAgricola\\src\\data\\Parcelas.csv"


-- | Registrar una nueva parcela, pidiendo información al usuario.
registrarParcela :: [Parcela] -> [Herramienta] -> IO [Parcela]
registrarParcela parcelas herramientas = do
    putStrLn "\n--- Registro de Parcela ---"
    nombre <- pedirInput "Ingrese el nombre de la parcela: "
    zona <- pedirInput "Ingrese la zona: "
    area <- pedirDouble "Ingrese el área de la parcela (en metros cuadrados): "
    vegetal <- pedirInput "Ingrese el nombre del producto a sembrar: "
    precio <- pedirInput "Ingrese el precio por kilo del vegetal: "
    mostrarHerramientas herramientas
    codsHerr <- pedirLista "Ingrese los códigos de herramientas a asignar (separados por comas): "
    let herramientasSeleccionadas = filter (\h -> H.codigo h `elem` codsHerr) herramientas
    codigoParcela <- generarCodigoParcela herramientas
    let nuevaParcela = Parcela codigoParcela nombre zona area vegetal precio herramientasSeleccionadas
    guardarParcela nuevaParcela
    return (parcelas ++ [nuevaParcela]) 


-- | Guardar una parcela en el archivo CSV
guardarParcela :: Parcela -> IO ()
guardarParcela parcela = do
    putStrLn "\nGuardando parcela..."
    existe <- doesFileExist filePath
    if not existe
        then do 
            writeFile filePath (parcelaToCSV parcela ++ "\n")
            putStrLn "Archivo creado y parcela guardada."
        else do
            appendFile filePath (parcelaToCSV parcela ++ "\n")
            putStrLn "Parcela guardada."
        
-- | Función para parsear el precio de cada vegetal en el formato: vegetal:precio
parsePrecio :: String -> Maybe (String, Double)
parsePrecio str =
    case break (== ':') str of
        (veg, ':' : precioStr) ->
            case reads precioStr of
                [(p, "")] -> Just (veg, p)
                _ -> Nothing
        _ -> Nothing

-- | Consultar una parcela por su código
consultarParcela :: [Parcela] -> IO ()
consultarParcela parcelas = do
    --Verificar lista de parcelas
    if null parcelas
        then do
            putStrLn "No hay parcelas registradas."
            return ()
        else do
            putStrLn "\n--- Consulta de Parcela ---"
            cod <- pedirInput "Ingrese el código de la parcela a consultar: "
            let parcelaEncontrada = buscarParcela cod parcelas
            case parcelaEncontrada of
                Just parcela -> do
                    mostrarParcela parcela
                    
                Nothing -> putStrLn "Parcela no encontrada."

-- | Buscar una parcela por su código
buscarParcela :: String -> [Parcela] -> Maybe Parcela
buscarParcela cod parcelas = 
    case filter (\p -> codigo p == cod) parcelas of
        [p] -> Just p
        _   -> Nothing

-- | Mostrar la información de una parcela
mostrarParcela :: Parcela -> IO ()
mostrarParcela parcela = do
    putStrLn $ "\nInformación de la Parcela:"
    putStrLn $ "Código: " ++ codigo parcela
    putStrLn $ "Nombre: " ++ nombre parcela
    putStrLn $ "Zona: " ++ zona parcela
    putStrLn $ "Área: " ++ show (area parcela) ++ " m²"
    putStrLn $ "Vegetal: " ++ vegetal parcela
    putStrLn $ "Precio por kilo: " ++ precio parcela
    putStrLn $ "Herramientas asignadas: " ++ intercalate ", " (map H.codigo (herramientas parcela))
    


-- | Leer las parcelas desde el archivo CSV
leerParcelas :: [Herramienta] -> IO [Parcela]
leerParcelas herramientasDisponibles = do
    existe <- doesFileExist filePath
    if not existe
        then return []
        else do
            contenido <- readFile filePath
            length contenido `seq` do
                let lineas = lines contenido
                    parcelas = map (flip csvToParcela herramientasDisponibles) lineas
                return parcelas

-- | Convertir una parcela a formato CSV
parcelaToCSV :: Parcela -> String
parcelaToCSV parcela =
    intercalate "," [
        codigo parcela,
        nombre parcela,
        zona parcela,
        show (area parcela),
        vegetal parcela,
        precio parcela,
        intercalate ";" (map H.codigo (herramientas parcela))  -- Guardar solamente el código de la herramienta
    ]

-- | Convertir una línea CSV a una parcela
csvToParcela :: String -> [Herramienta] -> Parcela
csvToParcela str herramientasDisponibles =
    let campos = split ',' str
        cod = campos !! 0
        nom = campos !! 1
        zon = campos !! 2
        are = read (campos !! 3) :: Double
        veg = campos !! 4
        pre = campos !! 5
        codHerramientas = split ';' (campos !! 6)
        herramientasSeleccionadas = mapMaybe (`buscarHerramienta` herramientasDisponibles) codHerramientas
    in Parcela cod nom zon are veg pre herramientasSeleccionadas



-- | Buscar una herramienta por su código
buscarHerramienta :: String -> [Herramienta] -> Maybe Herramienta
buscarHerramienta cod herramientas = 
    case filter (\h -> H.codigo h == cod) herramientas of
        [h] -> Just h
        _   -> Nothing

-- | Función auxiliar para dividir una cadena por un delimitador
split :: Char -> String -> [String]
split _ "" = []
split delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> split delim xs

-- | Eliminar espacios sobrantes
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Función auxiliar para pedir una cadena al usuario
pedirInput :: String -> IO String
pedirInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

-- | Función auxiliar para pedir un número decimal
pedirDouble :: String -> IO Double
pedirDouble prompt = do
    putStr prompt
    hFlush stdout
    readLn

-- | Función auxiliar para pedir una lista separada por comas
pedirLista :: String -> IO [String]
pedirLista prompt = do
    putStr prompt
    hFlush stdout
    entrada <- getLine
    return $ map trim $ splitOn "," entrada


-- | Función para generar un código único para cada parcela
-- Esta función puede ser mejorada para garantizar la unicidad, por ejemplo,
-- utilizando una base de datos o un contador persistente.
-- | Genera un nuevo código único para una parcela en formato "P####"
generarCodigoParcela :: [Herramienta] -> IO String
generarCodigoParcela herramientas = do
    putStrLn "Generando nuevo código de parcela..."
    parcelas <- leerParcelas herramientas  -- Esta función debe cargar las parcelas desde el archivo
    let codigos = map codigo parcelas  -- Extrae solo los códigos
        numeros = [read (drop 1 c) :: Int | c <- codigos, length c > 1]
        nuevoCodigo = if null numeros
                    then 1000
                    else maximum numeros + 1
    return ("P" ++ show nuevoCodigo)