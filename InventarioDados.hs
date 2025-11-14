-- Declara o nome do módulo (arquivo)
module InventarioDados
  ( Item(..)
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  ) where

import Data.Map (Map)
import Data.Time (UTCTime)

-- Representa um item do inventário.
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  }
  deriving (Show, Read, Eq)

-- Define Inventario como um Map de itemID (String) para Item
type Inventario = Map String Item

-- Define o tipo soma AcaoLog com as possíveis ações registradas no log data AcaoLog
data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  | List
  | Report
  deriving (Show, Read, Eq)

-- Define o resultado de uma operação: sucesso ou falha com uma mensagem
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

-- Define a entrada de log completa registrada na auditoria
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  }
  deriving (Show, Read, Eq)
