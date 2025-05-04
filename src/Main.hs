module Main where

import Controllers.Menu (mostrarMenuInicio)

main :: IO ()
main = do
    putStrLn "Iniciando el sistema..."
    mostrarMenuInicio