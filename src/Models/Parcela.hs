{-|
Module      : Models.Parcela
Description : Modelo de la estructura de una parcela de cultivo.
-}

module Models.Parcela (
    Parcela(..),
) where

import Models.Herramienta (Herramienta)  -- Importa el tipo Herramienta
import System.IO (hFlush, stdout)
-- | Estructura que define una parcela de cultivo
data Parcela = Parcela {
    codigo       :: String,            -- Código único de la parcela
    nombre       :: String,            -- Nombre de la parcela
    zona         :: String,            -- Zona donde se encuentra la parcela
    area         :: Double,            -- Área en metros cuadrados
    vegetal      :: String,            -- Tipos de vegetales sembrados
    precio       :: String,            -- Precio por kilo de cada vegetal
    herramientas :: [Herramienta]      -- Herramientas asociadas a la parcela
} deriving (Show)





