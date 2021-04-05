{-# LANGUAGE DeriveGeneric     #-}

--Gera todas as partidas de uma competição de pontos corridos.
--Aceita de 2 a 20 times. 
--Qualquer quantidade diferente retorna uma lista vazia de partidas.
--Aceita opção de jogo único ou casa e fora.

module PontosCorridos where

import Operacoes (embaralhar, converter, roundRobin, inverterRoundRobin)

import Data.List
import Data.Text (Text, pack)
import Data.Aeson hiding (json)

import GHC.Generics

data ModeloPontosCorridos = ModeloPontosCorridos {
    rodada :: Int,
    partidas :: [(Text, Text)]
} deriving (Generic, Show)

instance ToJSON ModeloPontosCorridos
instance FromJSON ModeloPontosCorridos

--Gera as rodadas - e suas respectivas partidas - a partir de uma quantidade
--de times e um booleano informando se possui partidas de ida e volta.
gerarRodadas :: Integral a => Bool -> a -> [[(a, a)]]
gerarRodadas True n = concat[ida, volta]
  where ida = roundRobin n
        volta = inverterRoundRobin ida
gerarRodadas False n = roundRobin n

--Pega as rodadas possíveis e substitui por times baseado em sua posição na lista.
substituirPorTimes :: [[(Int, Int)]] -> [b] -> [[(b, b)]]
substituirPorTimes [] _ = []
substituirPorTimes (a:as) timesEmbaralhados = 
  [(timesEmbaralhados !! (x-1), timesEmbaralhados !! (y-1)) | (x,y) <- a] 
  : substituirPorTimes as timesEmbaralhados


--Criar o modelo de uma competição de pontos corridos a partir de uma 
--lista de times e um booleano informando se possui partidas de ida e volta.
criarPontosCorridos :: Bool -> [Text] -> [ModeloPontosCorridos]
criarPontosCorridos temIdaVolta times
  | quantTimesInvalida = [ModeloPontosCorridos {
      rodada = 0,
      partidas = []
  }]
  | otherwise = [
    ModeloPontosCorridos {
      rodada = (x),
      partidas = rodadasComTimes !! (x-1)
    }| x <- [1..quantRodadas]]
  where
    quantTimes = length times
    rodadas = gerarRodadas temIdaVolta quantTimes
    quantRodadas = length rodadas
    timesEmbaralhados = converter (embaralhar times)
    rodadasComTimes = substituirPorTimes rodadas timesEmbaralhados
    quantTimesInvalida = (find (==quantTimes) [2..20]) == Nothing