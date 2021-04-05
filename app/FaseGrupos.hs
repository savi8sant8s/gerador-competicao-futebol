{-# LANGUAGE DeriveGeneric     #-}

--Gera os grupos e suas respectivas partidas de uma competição de fase de grupos.
--Aceita opção de jogo único ou casa e fora.
--Aceita opção de sortear times ou não.
--Aceita 8, 16 ou 32 times. 
--Qualquer quantidade diferente retorna uma lista vazia de partidas.

module FaseGrupos where

import Operacoes (dividir, converter, embaralhar, merge)
import PontosCorridos (ModeloPontosCorridos, criarPontosCorridos)

import Data.List 
import Data.List.Split
import Data.Text (Text, pack)
import Data.Aeson hiding (json)

import GHC.Generics

data ModeloFaseGrupos = ModeloFaseGrupos { 
    idGrupo:: Int, 
    grupo :: [Text], 
    partidas :: [ModeloPontosCorridos]
} deriving (Generic, Show)

instance ToJSON ModeloFaseGrupos
instance FromJSON ModeloFaseGrupos

--Define qual o sentido dos confrontos dos clubes que ficarem em 2º lugar.
confrontos2Posicao :: Integral a => a -> [a]
confrontos2Posicao quantGrupos = [ transforma x | x <-[1..quantGrupos]] 
    where 
        transforma n = if odd n then n+1 else n-1

--Define quais os confrontos dos times que se classificarem em 1º e 2º em cada grupo.
definirConfrontosClassificados :: Int -> [Text]
definirConfrontosClassificados quantGrupos = [pack ("1º do grupo " ++ show x ++ " enfrenta 2º do grupo " ++ show (visitante !! (x-1))) | x <-[1..quantGrupos]] 
    where 
        visitante = confrontos2Posicao quantGrupos 

--Gera os grupos da competição.
gerarGrupos :: [Text] -> [[Text]]
gerarGrupos times
    | quantTimesInvalida = []
    | otherwise = chunksOf 4 times
    where
        quantTimes = length times
        quantTimesInvalida = (find (==quantTimes) [8,16,32]) == Nothing

--Gera as partidas de cada grupo considerando se possui jogos de ida e volta.
gerarPartidas :: Bool -> [Text] -> [[ModeloPontosCorridos]]
--Obs.: o parâmetro sortear precisa estar como False
--pois a lógica de sorteio já é realizada nesse módulo.
gerarPartidas temIdaVolta times = [criarPontosCorridos temIdaVolta False x | x <- grupos]
    where grupos = gerarGrupos times

--Cria o modelo de uma competição de fase de grupos a partir de uma
--lista de times, opção de ida e volta e opção de sortear times ou não.
criarFaseGrupos :: Bool -> Bool -> [Text] -> [ModeloFaseGrupos]
criarFaseGrupos temIdaVolta sortear times
    | quantPartidas == 0 = []
    | otherwise = [ 
        ModeloFaseGrupos { 
            idGrupo = x+1,
            grupo = grupos !! x, 
            partidas = (partidas !! x)
        } | x <- [0..quantGrupos-1]]
    where 
      times' = if sortear == True then converter (embaralhar times) else times
      grupos = gerarGrupos times'
      partidas = gerarPartidas temIdaVolta times'
      quantPartidas = length partidas
      quantGrupos = length grupos