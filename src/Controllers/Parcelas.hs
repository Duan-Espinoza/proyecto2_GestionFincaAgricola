{-|
Módulo      : Controllers.Parcelas
Descripción : Proporciona funcionalidad para gestionar parcelas agrícolas.
Copyright   : (c) 2023
Licencia    : MIT
Mantenedor  : geovannniga32@gmail.com
Estabilidad : experimental
Portabilidad: POSIX

Este módulo contiene funciones para registrar, consultar y gestionar parcelas agrícolas 
utilizando un archivo CSV para almacenamiento persistente. También proporciona utilidades 
para interactuar con herramientas asociadas a las parcelas.

Funciones:
    - 'registrarParcela': Registra una nueva parcela interactuando con el usuario.
    - 'consultarParcela': Permite consultar una parcela por su código único.
    - 'leerParcelas': Lee todas las parcelas desde el archivo CSV.
    - 'csvToParcela': Convierte una línea CSV en un objeto 'Parcela'.

Utilidades:
    - 'guardarParcela': Guarda una parcela en el archivo CSV.
    - 'buscarParcela': Busca una parcela por su código.
    - 'mostrarParcela': Muestra información detallada de una parcela.
    - 'parcelaToCSV': Convierte un objeto 'Parcela' en una cadena CSV.
    - 'buscarHerramienta': Busca una herramienta por su código.
    - 'split': Divide una cadena por un delimitador dado.
    - 'trim': Elimina espacios en blanco al inicio y al final de una cadena.
    - 'pedirInput': Solicita al usuario una entrada de texto.
    - 'pedirDouble': Solicita al usuario una entrada numérica de tipo double.
    - 'pedirLista': Solicita al usuario una lista separada por comas.
    - 'generarCodigoParcela': Genera un código único para una nueva parcela.

Dependencias:
    - Models.Parcela: Define el tipo de datos 'Parcela'.
    - Models.Herramienta: Define el tipo de datos 'Herramienta' y funciones relacionadas.
    - Controllers.Herramientas: Proporciona funciones para gestionar herramientas.
    - System.IO, System.Directory, Data.List, Data.Maybe, Data.List.Split, Data.Char: Bibliotecas estándar de Haskell para operaciones de E/S, manipulación de cadenas y listas.

Notas:
    - El módulo asume la existencia de un archivo CSV en la ruta "src/data/Parcelas.csv".
    - La función 'generarCodigoParcela' genera códigos únicos en el formato "P####".
    - El manejo de errores es mínimo y debería mejorarse para uso en producción.
-}
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
filePath  = "src/data/Parcelas.csv"


{- Nombre: registrarParcela
   Descripción: Registra una nueva parcela en el sistema, solicitando al usuario la información necesaria.
   Parámetros:
       - parcelas: Lista de parcelas existentes.
       - herramientas: Lista de herramientas disponibles.
   Retorno: Devuelve la lista actualizada de parcelas.
   Excepciones: Maneja errores de entrada/salida y validación de datos.
   -}
registrarParcela :: [Parcela] -> [Herramienta] -> IO [Parcela]
registrarParcela parcelas herramientas = do
    -- Validar que la lista de herramientas no esté vacía
    if null herramientas
        then do
            putStrLn "\n*** No hay herramientas registradas. Por favor, registre herramientas primero ***"
            return parcelas
        else do
            
            -- Pedir información de la nueva parcela
            parcelas <- leerParcelas herramientas  -- Cargar parcelas existentes
            let codigosExistentes = map codigo parcelas  -- Obtener códigos existentes para evitar duplicados
            let codigosHerramientas = map H.codigo herramientas  -- Obtener códigos de herramientas disponibles

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
            return (parcelas ++ [nuevaParcela]) -- Agregar la nueva parcela a la lista existente


{-- Nombre: guardarParcela
    Descripción: Guarda una parcela en el archivo CSV.
    Parámetros:
        - parcela: Parcela a guardar.
    Retorno: IO () (acción de entrada/salida).
    Excepciones: Maneja errores de entrada/salida.
--}
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

{-
    Nombre: parsePrecio
    Descripción: Analiza una cadena de texto para extraer el nombre del vegetal y su precio.
    Parámetros:
        - str: Cadena de texto en formato "vegetal:precio".
    Retorno: Tal vez una tupla (nombre del vegetal, precio).
    Excepciones: Ninguna.
-}
parsePrecio :: String -> Maybe (String, Double)
parsePrecio str =
    case break (== ':') str of
        (veg, ':' : precioStr) ->
            case reads precioStr of
                [(p, "")] -> Just (veg, p)
                _ -> Nothing
        _ -> Nothing

{-
Nombre: consultarParcela
Descripción: Permite al usuario consultar una parcela por su código.
Parámetros:
    - parcelas: Lista de parcelas registradas.
Retorno: IO () (acción de entrada/salida).
Excepciones: Maneja errores de entrada/salida.
-}
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

{-
Nombre: buscarParcela
Descripción: Busca una parcela por su código en una lista de parcelas.
Parámetros:
    - cod: Código de la parcela a buscar.
    - parcelas: Lista de parcelas.

Retorno: Parcela encontrada o Nothing si no se encuentra.
Excepciones: Ninguna.
-}
buscarParcela :: String -> [Parcela] -> Maybe Parcela
buscarParcela cod parcelas = 
    case filter (\p -> codigo p == cod) parcelas of
        [p] -> Just p
        _   -> Nothing

{-
Nombre: mostrarParcela
Descripción: Muestra la información de una parcela en la consola.
Parámetros:
    - parcela: Parcela a mostrar.
Retorno: IO () (acción de entrada/salida).
-}
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
    


{-
Nombre: leerParcelas
Descripción: Lee las parcelas desde el archivo CSV y las convierte en objetos Parcela.

Parámetros:
    - herramientasDisponibles: Lista de herramientas disponibles para asociar a las parcelas.
Retorno: IO [Parcela] (lista de parcelas leídas desde el archivo).
Excepciones: Maneja errores de entrada/salida.
-}
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

{-
Nombre: parcelaToCSV
Descripción: Convierte un objeto Parcela a una cadena CSV.
Parámetros:
    - parcela: Parcela a convertir.
Retorno: Cadena CSV representando la parcela.
-}
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

{-
Nombre: csvToParcela
Descripción: Convierte una línea CSV en un objeto Parcela.
Parámetros:
    - str: Línea CSV que representa una parcela.
    - herramientasDisponibles: Lista de herramientas disponibles para asociar a la parcela.
    Retorno: Parcela construida a partir de la línea CSV.
-}
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


{-
Nombre: buscarHerramienta
Descripción: Busca una herramienta por su código en una lista de herramientas.

Parámetros:
    - cod: Código de la herramienta a buscar.
    - herramientas: Lista de herramientas.
Retorno: Herramienta encontrada o Nothing si no se encuentra.
Excepciones: Ninguna.
-}
buscarHerramienta :: String -> [Herramienta] -> Maybe Herramienta
buscarHerramienta cod herramientas = 
    case filter (\h -> H.codigo h == cod) herramientas of
        [h] -> Just h
        _   -> Nothing

{-
Nombre: split
Descripción: Divide una cadena en una lista de cadenas utilizando un delimitador.

Parámetros:
    - delim: Carácter delimitador.
    - str: Cadena a dividir.
Retorno: Lista de cadenas resultantes de la división.
-}
split :: Char -> String -> [String]
split _ "" = []
split delim str =
    let (first, rest) = break (== delim) str
    in first : case rest of
        [] -> []
        (_:xs) -> split delim xs

{-
Nombre: trim
Descripción: Elimina espacios en blanco al inicio y al final de una cadena.
Parámetros:
    - str: Cadena a limpiar.
    
Retorno: Cadena sin espacios en blanco al inicio y al final.
-}
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

{-
Nombre: pedirInput
Descripción: Solicita al usuario una entrada de texto.
Parámetros:
    - prompt: Mensaje a mostrar al usuario.
    
Retorno: Cadena de texto ingresada por el usuario.
-}
pedirInput :: String -> IO String
pedirInput prompt = do
    putStr prompt
    hFlush stdout
    getLine
{-
Nombre: pedirDouble
Descripción: Solicita al usuario una entrada numérica de tipo double.
Parámetros:
    - prompt: Mensaje a mostrar al usuario.
    
Retorno: Valor numérico de tipo double ingresado por el usuario.
-}
pedirDouble :: String -> IO Double
pedirDouble prompt = do
    putStr prompt
    hFlush stdout
    readLn

{-
Nombre: pedirLista

Descripción: Solicita al usuario una entrada de texto en forma de lista separada por comas.
Parámetros:
    - prompt: Mensaje a mostrar al usuario.
Retorno: Lista de cadenas ingresadas por el usuario.
-}
pedirLista :: String -> IO [String]
pedirLista prompt = do
    putStr prompt
    hFlush stdout
    entrada <- getLine
    return $ map trim $ splitOn "," entrada


{-
Nombre: generarCodigoParcela
Descripción: Genera un nuevo código único para una parcela.
Parámetros:
    - herramientas: Lista de herramientas disponibles.
    
Retorno: Un nuevo código de parcela en formato de cadena.
-}
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