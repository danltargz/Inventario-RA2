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

addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem tempo item inv
  | Map.member (itemID item) inv =
      Left "Erro: item já existe no inventário."
  | otherwise =
      let novoInv = Map.insert (itemID item) item inv
          logEntry = LogEntry tempo Add
            ("Item adicionado: " ++ nome item ++ " (ID: " ++ itemID item ++ ")")
            Sucesso
      in Right (novoInv, logEntry)            