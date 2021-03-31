{-# LANGUAGE DeriveGeneric     #-}

--Gera os confrontos de uma competição no modelo de mata-mata (eliminatória).
--Aceita 4,8,16 ou 32 times. 
--Qualquer quantidade diferente retorna uma lista vazia de partidas.
--Aceita opção de jogo único ou casa e fora.

module Eliminatoria where

import Operacoes (dividir, converter, embaralhar)
import Data.List
import Data.Text (Text, pack)
import Data.Aeson hiding (json)
import GHC.Generics

data Fase = DezesseisAvos | Oitavas | Quartas | Semifinal
        deriving (Generic, Show)

instance ToJSON Fase
instance FromJSON Fase

data ModeloEliminatoria = ModeloEliminatoria { 
    disputa:: Int, 
    fase :: Fase, 
    casa :: (Text, Text), 
    fora :: (Text, Text)
} deriving (Generic, Show)

instance ToJSON ModeloEliminatoria
instance FromJSON ModeloEliminatoria

--Converte uma quantidade de times para um modelo de eliminatória.
paraElim :: Int -> Fase
paraElim 16 = DezesseisAvos
paraElim 8  = Oitavas
paraElim 4  = Quartas
paraElim 2  = Semifinal

--Gera as partidas das eliminatórias sorteando os times.
gerarPartidasElim :: [Text] -> [(Text, Text)]
gerarPartidasElim times 
            | quantTimesInvalida = []
            | otherwise = zip (fst timesDivididos) (snd timesDivididos)
            where 
              quantTimes = length times
              quantTimesInvalida = (find (==quantTimes) [4,8,16,32]) == Nothing
              timesEmbaralhados = converter (embaralhar times)
              timesDivididos = dividir timesEmbaralhados

--Define qual é a descrição da fase eliminatória a partir da quantidade de partidas.
definirFaseElim :: Int -> [(Fase)]
definirFaseElim quantPartidas = [(paraElim quantPartidas) | x <- [1..quantPartidas]]
      
--Cria o modelo de uma competição eliminatória a partir de uma lista de
--times e um booleano informando se possui partidas de ida e volta.
criarEliminatoria :: Bool -> [Text] -> [ModeloEliminatoria]
criarEliminatoria temIdaVolta times
    | partidas == [] = []
    | temIdaVolta == True = [ 
        ModeloEliminatoria { 
            disputa = x+1,
            fase = (fase !! x), 
            casa = (partidas !! x),
            fora = (snd (partidas !! x), fst (partidas !! x))
        } | x <- [0..quantPartidas-1]]
    | otherwise = [ 
        ModeloEliminatoria { 
            disputa = x+1,
            fase = (fase !! x), 
            casa = (partidas !! x),
            fora = (pack "", pack "")
        } | x <- [0..quantPartidas-1]]
    where 
      partidas = gerarPartidasElim times
      quantPartidas = length partidas
      fase = definirFaseElim quantPartidas