-- Modelo Trabajador 
-- Atributos: Cedula, Nombre Completo, Rol
module Models.Trabajador
  ( Trabajador(..)
  , trabajadoresIniciales
  , buscarTrabajadorPorCedula
  ) where

-- Definición del tipo Trabajador
data Trabajador = Trabajador
  { cedula :: String
  , nombre :: String
  , rol    :: String
  } deriving (Show, Eq)

-- Lista inicial de trabajadores registrados
trabajadoresIniciales :: [Trabajador]
trabajadoresIniciales =
  [ Trabajador "101010101" "Geovanni González" "Agrónomo"
  , Trabajador "202020202" "Antonio Gutierrez" "Supervisor"
  , Trabajador "303030303" "Moises Duran" "Operario"
  , Trabajador "404040404" "Dariel Mendez" "Operario"
  , Trabajador "505050505" "Nehemias Flores" "Supervisor"
  ]

-- Buscar un trabajador por su cédula
buscarTrabajadorPorCedula :: String -> [Trabajador] -> Maybe Trabajador
buscarTrabajadorPorCedula ced lista = 
  case filter (\t -> cedula t == ced) lista of
    (x:_) -> Just x
    []    -> Nothing