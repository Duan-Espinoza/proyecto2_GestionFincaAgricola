{-|
Módulo      : Models.Cosecha
Descripción : Este módulo define las estructuras de datos y funciones relacionadas con la gestión de cosechas en una finca agrícola.

Tipos de datos:
- 'EstadoCosecha': Representa los posibles estados de una cosecha. Puede ser:
    - 'Planificada': La cosecha está planificada pero aún no ha comenzado.
    - 'EnCurso': La cosecha está en progreso.
    - 'Completada': La cosecha ha finalizado.
    - 'Cancelada': La cosecha ha sido cancelada.
- 'Cosecha': Representa una cosecha con los siguientes campos:
    - 'idCosecha': Identificador único de la cosecha.
    - 'trabajadorId': Identificador del trabajador responsable.
    - 'parcelaId': Identificador de la parcela donde se realiza la cosecha.
    - 'fechaInicio': Fecha de inicio de la cosecha.
    - 'fechaFin': Fecha de finalización de la cosecha.
    - 'vegetal': Tipo de vegetal cosechado.
    - 'cantidad': Cantidad cosechada (en unidades específicas).
    - 'estado': Estado actual de la cosecha ('EstadoCosecha').

Funciones:
- 'solapamientoFechas': Determina si dos cosechas tienen fechas que se solapan.
    - Entrada: Dos valores de tipo 'Cosecha'.
    - Salida: 'True' si las fechas de las cosechas se solapan, 'False' en caso contrario.

- 'parsearFecha': Convierte una cadena de texto en una fecha ('Day') utilizando el formato "YYYY-MM-DD".
    - Entrada: Una cadena de texto.
    - Salida: 'Just Day' si la conversión es exitosa, 'Nothing' en caso contrario.

- 'cosechaToCSV': Convierte un registro de tipo 'Cosecha' en una cadena de texto en formato CSV.
    - Entrada: Un valor de tipo 'Cosecha'.
    - Salida: Una cadena de texto con los campos separados por comas.

- 'csvToCosecha': Convierte una cadena de texto en formato CSV en un registro de tipo 'Cosecha'.
    - Entrada: Una cadena de texto en formato CSV.
    - Salida: 'Just Cosecha' si la conversión es exitosa, 'Nothing' en caso contrario.

- 'split': Divide una cadena de texto en una lista de cadenas utilizando un delimitador específico.
    - Entrada: Un carácter delimitador y una cadena de texto.
    - Salida: Una lista de cadenas de texto.

- 'readMaybe': Intenta convertir una cadena de texto en un valor de tipo 'Read'.
    - Entrada: Una cadena de texto.
    - Salida: 'Just a' si la conversión es exitosa, 'Nothing' en caso contrario.
-}
-- src/Models/Cosecha.hs
module Models.Cosecha (
    Cosecha(..),
    EstadoCosecha(..),
    parsearFecha,
    csvToCosecha,
    cosechaToCSV,
    solapamientoFechas
) where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

data EstadoCosecha = Planificada | EnCurso | Completada | Cancelada
    deriving (Show, Read, Eq)

data Cosecha = Cosecha {
    idCosecha :: String,
    trabajadorId :: String,
    parcelaId :: String,
    fechaInicio :: Day,
    fechaFin :: Day,
    vegetal :: String,
    cantidad :: Double,
    estado :: EstadoCosecha
} deriving (Show, Eq)

solapamientoFechas :: Cosecha -> Cosecha -> Bool
solapamientoFechas c1 c2 = 
    (fechaInicio c1 <= fechaFin c2) && (fechaFin c1 >= fechaInicio c2)

parsearFecha :: String -> Maybe Day
parsearFecha = parseTimeM True defaultTimeLocale "%Y-%m-%d"

cosechaToCSV :: Cosecha -> String
cosechaToCSV c = intercalate "," [
    idCosecha c,
    trabajadorId c,
    parcelaId c,
    show (fechaInicio c),
    show (fechaFin c),
    vegetal c,
    show (cantidad c),
    show (estado c)
    ]

csvToCosecha :: String -> Maybe Cosecha
csvToCosecha linea = case split ',' linea of
    [idC, tid, pid, fini, ffin, veg, cant, est] -> 
        case (parsearFecha fini, parsearFecha ffin, readMaybe cant, readMaybe est) of
            (Just fi, Just ff, Just c, Just e) -> 
                Just Cosecha
                    { idCosecha = idC
                    , trabajadorId = tid
                    , parcelaId = pid
                    , fechaInicio = fi
                    , fechaFin = ff
                    , vegetal = veg
                    , cantidad = c
                    , estado = e
                    }
            _ -> Nothing
    _ -> Nothing

split :: Char -> String -> [String]
split delim = foldr (\c acc -> 
    if c == delim 
        then []:acc 
        else (c:head acc):tail acc
    ) [[]]

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing