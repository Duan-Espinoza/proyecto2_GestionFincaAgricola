{-|
Module      : Models.Herramienta
Description : Define el modelo de datos para representar herramientas y funciones auxiliares.
Copyright   : (c) 2023
License     : MIT
Maintainer  : geovanniga32@gmail.com
Stability   : experimental
Portability : portable

Este módulo define el tipo de datos 'Herramienta', que representa una herramienta utilizada en la finca agrícola. 
También incluye una función para comparar herramientas según su código, útil para verificar unicidad.

== Tipos Exportados

[Herramienta]
  Representa una herramienta con los siguientes atributos:

  * 'codigo' :: 'String' — Código único que identifica a la herramienta.
  * 'nombre' :: 'String' — Nombre de la herramienta.
  * 'descripcion' :: 'String' — Descripción detallada del uso o características de la herramienta.
  * 'tipo' :: 'String' — Tipo o categoría de la herramienta (ej. "Manual", "Eléctrica").

== Funciones Exportadas

['igualCodigo']
  Compara dos herramientas y determina si tienen el mismo código.
-}

module Models.Herramienta 
    (   
        Herramienta(..)
      , igualCodigo
    ) where

-- | Representa una herramienta utilizada en las operaciones de la finca agrícola.
data Herramienta = Herramienta 
  { codigo      :: String  -- ^ Código único de la herramienta.
  , nombre      :: String  -- ^ Nombre identificativo.
  , descripcion :: String  -- ^ Descripción detallada de la herramienta.
  , tipo        :: String  -- ^ Categoría o tipo de herramienta.
  } deriving (Show,Read, Eq)

-- | Compara dos herramientas por su código.
--
-- == Parámetros:
-- 
-- * @h1@ — Primera herramienta.
-- * @h2@ — Segunda herramienta.
--
-- == Retorno:
--
-- * 'True' si ambas herramientas tienen el mismo código.
-- * 'False' si los códigos son diferentes.
--
-- Esta función es útil para validar unicidad en registros.
igualCodigo :: Herramienta -> Herramienta -> Bool
igualCodigo h1 h2 = codigo h1 == codigo h2