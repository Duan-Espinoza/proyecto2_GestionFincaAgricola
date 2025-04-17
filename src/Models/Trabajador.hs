{-|
Module      : Models.Trabajador
Description : Define el modelo de datos para representar a un trabajador y funciones relacionadas.
Copyright   : (c) 2023
License     : MIT
Maintainer  : geovanniga32@gmail.com
Stability   : experimental
Portability : portable

Este módulo define el tipo de datos 'Trabajador', que representa a un trabajador del sistema de gestión 
de una finca agrícola. Además, incluye una lista inicial de trabajadores registrados y una función para 
buscar trabajadores por su número de cédula.

== Tipos Exportados

[Trabajador] 
  Tipo de datos que representa a un trabajador. Incluye:
  
  * 'cedula' :: 'String' — Cédula de identidad única del trabajador.
  * 'nombre' :: 'String' — Nombre completo del trabajador.
  * 'rol'    :: 'String' — Rol que desempeña el trabajador en la finca (por ejemplo: "Agrónomo", "Supervisor", etc.).

== Funciones Exportadas

['trabajadoresIniciales']
  Lista de trabajadores predefinida con la que se puede inicializar el sistema.

['buscarTrabajadorPorCedula']
  Permite buscar un trabajador en una lista dada su cédula.
-}

module Models.Trabajador
  ( Trabajador(..)
  , trabajadoresIniciales
  , buscarTrabajadorPorCedula
  ) where

-- | Representa a un trabajador de la finca con su cédula, nombre completo y rol.
data Trabajador = Trabajador
  { cedula :: String  -- ^ Cédula de identidad única.
  , nombre :: String  -- ^ Nombre completo del trabajador.
  , rol    :: String  -- ^ Rol o cargo que desempeña en la finca.
  } deriving (Show, Eq)

-- | Lista inicial de trabajadores registrados en el sistema.
-- Esta lista puede ser usada como punto de partida para la ejecución de la aplicación.
trabajadoresIniciales :: [Trabajador]
trabajadoresIniciales =
  [ Trabajador "101010101" "Geovanni González" "Agrónomo"
  , Trabajador "202020202" "Antonio Gutierrez" "Supervisor"
  , Trabajador "303030303" "Moises Duran" "Operario"
  , Trabajador "404040404" "Dariel Mendez" "Operario"
  , Trabajador "505050505" "Nehemias Flores" "Supervisor"
  ]

-- | Busca un trabajador en una lista a partir de su cédula.
--
-- == Parámetros:
--
-- * @ced@ — La cédula del trabajador que se desea buscar.
-- * @[Trabajador]@ — La lista de trabajadores en la cual se realiza la búsqueda.
--
-- == Retorno:
--
-- * @Just Trabajador@ — Si se encuentra un trabajador con la cédula dada.
-- * @Nothing@ — Si no se encuentra ningún trabajador con esa cédula.
buscarTrabajadorPorCedula :: String -> [Trabajador] -> Maybe Trabajador
buscarTrabajadorPorCedula ced lista = 
  case filter (\t -> cedula t == ced) lista of
    (x:_) -> Just x
    []    -> Nothing
