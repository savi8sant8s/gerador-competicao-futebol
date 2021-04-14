{-# LANGUAGE OverloadedStrings #-}

--Responsável por registrar as requisições
--realizadas nas APIs.

module BancoDados where

import Data.Text (Text, pack)
import Data.Time

import qualified Data.Text as T

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok

data Requisicao = Requisicao { 
  timestamp :: UTCTime,
  competicao :: Text
} deriving (Eq,Read,Show)

instance FromRow Requisicao where
  fromRow = Requisicao <$> field <*> field

instance ToRow Requisicao where
  toRow (Requisicao pTimestamp pCompeticao) = toRow (pTimestamp, pCompeticao)

registrarRequisicao timestamp competicao = do
    conn <- open "requisicoes.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS requisicoes (id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, timestamp BLOB NOT NULL, competicao TEXT NOT NULL)"
    execute conn "INSERT INTO requisicoes (timestamp, competicao) VALUES (?,?)" (Requisicao timestamp (pack competicao))
    close conn