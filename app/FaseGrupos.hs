{-# LANGUAGE DeriveGeneric     #-}

--Gera os grupos e suas respectivas partidas de uma competição de fase de grupos.
--Aceita de 8, 16 e 32 times. 
--Qualquer quantidade diferente retorna uma lista vazia de partidas.
--Aceita opção de jogo único ou casa e fora.

module FaseGrupos where

import Operacoes (dividir, converter, embaralhar,merge)
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
gerarGrupos timesEmbaralhados
    | quantTimesInvalida = []
    | otherwise = chunksOf 4 timesEmbaralhados
    where
        quantTimes = length timesEmbaralhados
        quantTimesInvalida = (find (==quantTimes) [8,16,32]) == Nothing

--Gera as partidas de cada grupo considerando se possui jogos de ida e volta ou só ida.
gerarPartidas :: Bool -> [Text] -> [[ModeloPontosCorridos]]
gerarPartidas temIdaVolta timesEmbaralhados = [criarPontosCorridos temIdaVolta x | x <- grupos]
    where grupos = gerarGrupos timesEmbaralhados

--Cria o modelo de uma competição de fase de grupos a partir de uma
--lista de times e um booleano informando se possui partidas de ida e volta.
criarFaseGrupos :: Bool -> [Text] -> [ModeloFaseGrupos]
criarFaseGrupos temIdaVolta times
    | quantPartidas == 0 = []
    | otherwise = [ 
        ModeloFaseGrupos { 
            idGrupo = x+1,
            grupo = grupos !! x, 
            partidas = (partidas !! x)
        } | x <- [0..quantGrupos-1]]
    where 
      timesEmbaralhados = converter (embaralhar times)
      grupos = gerarGrupos timesEmbaralhados
      partidas = gerarPartidas temIdaVolta timesEmbaralhados
      quantPartidas = length partidas
      quantGrupos = length grupos