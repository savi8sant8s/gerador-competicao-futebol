{-# LANGUAGE ScopedTypeVariables #-}

--Responsável por realizar algumas operações
--pertinentes aos demais módulos.

module Operacoes where

import System.Random
import Data.Array.IO
import Control.Monad
import Data.STRef
import System.IO.Unsafe    

--Pega uma lista de times e embaralha suas posições na lista.                            
embaralhar :: [a] -> IO [a]
embaralhar lista = do
        ar <- newArray n lista
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length lista
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n lista =  newListArray (1,n) lista
    
--Pega uma lista e divide em duas.
dividir :: [a] -> ([a],[a])
dividir lista = splitAt (((length lista) + 1) `div` 2) lista

--Pega uma mônada e extrai o tipo interno dela.
converter :: IO a -> a
converter valor = unsafePerformIO valor

--Pega duas listas e transforma em uma.
merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs
