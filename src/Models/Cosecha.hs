{-|
Module      : Models.Cosecha
Description : Modelo de datos para las cosechas y funciones relacionadas.
-}

module Models.Cosecha (
    Cosecha(..),
    EstadoCosecha(..),
    generarCosechaId,
    validarDisponibilidadParcela
) where

import Models.Parcela (Parcela)
import Data.Time (Day, fromGregorian, diffDays)
import Data.List (find)

-- Estados posibles de una cosecha
data EstadoCosecha = Pendiente | Cerrada deriving (Show, Eq)

-- Estructura de una cosecha
data Cosecha = Cosecha {
    idCosecha :: String,          -- Formato "COS-001"
    trabajadorId :: String,       -- Cédula del trabajador
    parcelaId :: String,          -- Código de parcela
    fechaInicio :: String,        -- Formato "YYYY-MM-DD"
    fechaFin :: String,
    vegetal :: String,
    cantidadKg :: Double,
    estado :: EstadoCosecha
} deriving (Show, Eq)

-- Generar ID único (ej: "COS-001")
generarCosechaId :: [Cosecha] -> String
generarCosechaId cosechas =
    let maxId = maximum [read (drop 4 id) :: Int | Cosecha {idCosecha = id} <- cosechas]
    in "COS-" ++ show (maxId + 1)

-- Validar si una parcela está disponible en un rango de fechas
validarDisponibilidadParcela :: String -> [Cosecha] -> (String, String) -> Bool
validarDisponibilidadParcela parcelaId cosechas (inicio, fin) =
    not $ any (solapaCon (inicio, fin)) [ (fechaInicio c, fechaFin c) | c <- cosechas, parcelaId c == parcelaId ]

-- Función auxiliar para comparar rangos de fechas
solapaCon :: (String, String) -> (String, String) -> Bool
solapaCon (inicio1, fin1) (inicio2, fin2) =
    (inicio1 <= fin2) && (inicio2 <= fin1)