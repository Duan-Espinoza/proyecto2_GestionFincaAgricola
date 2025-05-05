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