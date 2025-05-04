{-|
Module      : Controllers.Herramientas
Description : Módulo para cargar y gestionar herramientas agrícolas desde un archivo.
Copyright   : (c) 2025
License     : MIT
Maintainer  : geovanniga32@gmail.com
Stability   : experimental
Portability : portable

Este módulo contiene las funciones necesarias para cargar herramientas agrícolas desde un archivo
y gestionarlas dentro del sistema. El archivo debe tener las herramientas en formato CSV con campos 
separados por comas.

== Funcionalidades

* Cargar herramientas desde un archivo y añadirlas a la lista existente.
* Validación para evitar la duplicación de herramientas basadas en su código.
* Mostrar la lista de herramientas cargadas.

-}

module Controllers.Herramientas (
    cargarHerramientasDesdeArchivo
    , mostrarHerramientas
) where

-- Importaciones
import Models.Herramienta
import System.IO
import System.Directory (doesFileExist)
import Data.List (nubBy)

-- | Parsea una línea del archivo y la convierte en una herramienta.
-- 
-- Esta función toma una línea de texto en formato CSV (separada por comas) y la convierte en una 
-- instancia de la estructura `Herramienta`. Si la línea tiene el formato correcto, la función devuelve 
-- una herramienta, de lo contrario, devuelve `Nothing`.
--
-- @param linea Línea del archivo a ser parseada.
-- @return Una herramienta `Maybe Herramienta` (puede ser `Nothing` si la línea no es válida).
parsearHerramienta :: String -> Maybe Herramienta
parsearHerramienta linea =
    case splitBy ',' linea of
        [c, n, d, t] -> Just $ Herramienta c n d t
        _            -> Nothing

-- | Función auxiliar para separar una cadena de texto por un delimitador.
-- 
-- Toma un delimitador y una cadena de texto, y la divide en una lista de cadenas. 
-- En este caso, se utiliza para dividir la línea del archivo en sus componentes (código, nombre, descripción, tipo).
--
-- @param delim Delimitador (en este caso una coma).
-- @param str Cadena de texto a separar.
-- @return Lista de cadenas resultantes de la división.
splitBy :: Char -> String -> [String]
splitBy delim = foldr f [[]]
    where
        f c l@(x:xs)
            | c == delim = []:l
            | otherwise  = (c:x):xs

-- | Carga herramientas desde un archivo y las muestra.
--
-- Esta función solicita la ruta del archivo, verifica si existe y luego carga las herramientas
-- desde el archivo. Si el archivo es válido, las herramientas son parseadas y añadidas a la lista
-- actual de herramientas. Además, se asegura de que no haya duplicados en la lista basándose en el código
-- de cada herramienta. Al finalizar, se muestra una lista con las herramientas nuevas registradas 
-- y una lista total de todas las herramientas registradas.
--
-- @param actuales Lista actual de herramientas.
-- @return Lista actualizada de herramientas después de la carga desde el archivo.
cargarHerramientasDesdeArchivo :: [Herramienta] -> IO [Herramienta]
cargarHerramientasDesdeArchivo actuales = do
    putStr "\nIngrese la ruta del archivo: "
    hFlush stdout
    ruta <- getLine
    existe <- doesFileExist ruta
    if not existe
        then do
            putStrLn "\n[!] El archivo no existe. Intente nuevamente."
            return actuales
        else do
            contenido <- readFile ruta
            let lineas = lines contenido
            let nuevasPosibles = map parsearHerramienta lineas
            let nuevasValidas = [h | Just h <- nuevasPosibles]
            let unicos = filter (\h -> notElem (codigo h) (map codigo actuales)) nuevasValidas
            let actualizadas = nubBy igualCodigo (actuales ++ unicos)
            
            putStrLn "\nLista total de herramientas registradas:"
            mapM_ mostrarHerramienta actualizadas
            return actualizadas

-- | Muestra la información de una herramienta en la consola.
-- Esta función imprime los detalles de una herramienta, incluyendo su código, nombre, descripción y tipo.
-- @param h La herramienta que se desea mostrar.
-- @return Herramienta mostrada.
--
mostrarHerramienta :: Herramienta -> IO ()
mostrarHerramienta h = do
    putStrLn $ "- Código: " ++ codigo h
    putStrLn $ "  Nombre: " ++ nombre h
    putStrLn $ "  Descripción: " ++ descripcion h
    putStrLn $ "  Tipo: " ++ tipo h
    putStrLn ""

-- | Mostrar todas las herramientas registradas en el sistema.
-- Esta función imprime la lista de herramientas registradas
--
-- @param herramientas Lista de herramientas a mostrar.
-- @return Lista de herramientas mostradas.
mostrarHerramientas :: [Herramienta] -> IO ()
mostrarHerramientas herramientas = do
    putStrLn "\nLista de herramientas registradas:"
    mapM_ mostrarHerramienta herramientas

