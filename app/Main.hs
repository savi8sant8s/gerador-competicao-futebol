{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Eliminatoria (ModeloEliminatoria, criarEliminatoria)
import FaseGrupos (ModeloFaseGrupos, criarFaseGrupos, definirConfrontosClassificados)
import PontosCorridos (ModeloPontosCorridos, criarPontosCorridos)

import Web.Spock
import Web.Spock.Config

import Data.List
import Data.Time
import Data.Aeson hiding (json)
import Data.Text (Text, pack)

import GHC.Generics
import System.IO.Unsafe    

data Times = Times {
  temIdaVolta:: Bool,
  sortear:: Bool,
  times :: [Text]
} deriving (Generic, Show)

instance ToJSON Times
instance FromJSON Times

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

app :: Api
app = prehook corsHeader $
  do     
    post "v1/gerar-competicao/mata-mata" $ do
      times' <- jsonBody' :: ApiAction Times
      let partidas = criarEliminatoria (temIdaVolta times') (sortear times') (times times')
      let timestamp = unsafePerformIO getCurrentTime
      let mataMata = object [ "timestamp" .= timestamp, 
                              "tipo" .= pack "Eliminatória",
                              "partidas" .= partidas ]
      json mataMata

    post "v1/gerar-competicao/fase-grupos" $ do
      times' <- jsonBody' :: ApiAction Times
      let partidas = criarFaseGrupos (temIdaVolta times') (sortear times') (times times')
      let quantGrupos = (length (times times')) `div` 4
      let segundaFase = if (find (==quantGrupos) [2,4,8]) == Nothing then [] else definirConfrontosClassificados quantGrupos
      let timestamp = unsafePerformIO getCurrentTime
      let faseGrupos = object [ "timestamp" .= timestamp, 
                                "tipo" .= pack "Fase de Grupos",
                                "grupos" .= partidas,
                                "segundaFase" .= segundaFase ]
      json faseGrupos

    post "v1/gerar-competicao/pontos-corridos" $ do
      times' <- jsonBody' :: ApiAction Times
      let partidas = criarPontosCorridos (temIdaVolta times') (sortear times') (times times')
      let timestamp = unsafePerformIO getCurrentTime
      let pontosCorridos = object [ "timestamp" .= timestamp, 
                                    "tipo" .= pack "Pontos corridos",
                                    "partidas" .= partidas ]
      json pontosCorridos
    
      
