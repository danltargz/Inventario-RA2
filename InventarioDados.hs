module InventarioDados
  ( Item(..)
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  ) where

import Data.Map (Map)
import Data.Time (UTCTime)

data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  }
  deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  | List
  | Report
  deriving (Show, Read, Eq)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  }
  deriving (Show, Read, Eq)
