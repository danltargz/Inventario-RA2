module InventarioLogica
  ( addItem
  , removeItem
  , updateQty
  , ResultadoOperacao
  ) where

import InventarioDados
import qualified Data.Map as Map
import Data.Time (UTCTime)

type ResultadoOperacao = (Inventario, LogEntry)
