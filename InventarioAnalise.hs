module InventarioAnalise
  ( logsDeErro
  , historicoPorItem
  , itemMaisMovimentado
  ) where

import InventarioDados
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (isInfixOf, isPrefixOf)

-- filtra e retorna apenas os logs que possuem status de falha
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro (LogEntry _ _ _ (Falha _)) = True
    isErro _ = False

-- retorna historico de operacoes agrupadas por item
-- recebe [LogEntry] e retorna Map onde a chave e o itemID e o valor e a lista de operacoes
historicoPorItem :: [LogEntry] -> Map String [LogEntry]
historicoPorItem logs = 
  let logsComID = [(extrairItemID le, le) | le <- logs]
      logsValidos = [(itemId, logEntry) | (Just itemId, logEntry) <- logsComID]
      agrupados = Map.fromListWith (++) [(itemId, [logEntry]) | (itemId, logEntry) <- logsValidos]
  in agrupados

-- identifica o item com mais operacoes registradas nos logs
-- retorna Nothing se nao houver logs ou Just (itemID, quantidadeDeOperacoes)
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let historico = historicoPorItem logs
      contagens = Map.toList $ Map.map length historico
  in case contagens of
       [] -> Nothing
       _ -> Just $ maximoPor snd contagens
  where
    maximoPor f xs = foldl1 (\a b -> if f a >= f b then a else b) xs

-- extrai o ID do item a partir de um LogEntry analisando o campo detalhes e acao
-- retorna Nothing se nao conseguir extrair um ID valido
extrairItemID :: LogEntry -> Maybe String
extrairItemID (LogEntry _ acao det _) = 
  case acao of
    Add -> extrairDeAdd det
    Remove -> extrairDeRemove det
    Update -> extrairDeUpdate det
    _ -> Nothing
  where
    -- para Add procura por (ID: e extrai ate o )
    extrairDeAdd :: String -> Maybe String
    extrairDeAdd s = 
      case dropWhile (/= '(') s of
        ('(':resto) -> 
          case dropWhile (/= ':') resto of
            (':':' ':resto2) -> 
              let itemId = takeWhile (/= ')') resto2
              in if null itemId then Nothing else Just itemId
            _ -> Nothing
        _ -> Nothing
    
    -- para Remove procura por do item ou ID=
    extrairDeRemove :: String -> Maybe String
    extrairDeRemove s
      | "do item " `isInfixOf` s = 
          let partes = words s
              idx = encontrarIndice "item" partes
          in if idx >= 0 && idx + 1 < length partes
             then Just (partes !! (idx + 1))
             else Nothing
      | "ID=" `isInfixOf` s =
          case dropWhile (/= '=') (dropWhile (/= 'I') s) of
            ('=':resto) -> Just (takeWhile (\c -> c /= ' ' && c /= ')') resto)
            _ -> Nothing
      | otherwise = Nothing
    
    -- para Update procura por do item ou ID=
    extrairDeUpdate :: String -> Maybe String
    extrairDeUpdate s
      | "do item " `isInfixOf` s =
          let partes = words s
              idx = encontrarIndice "item" partes
          in if idx >= 0 && idx + 1 < length partes
             then Just (partes !! (idx + 1))
             else Nothing
      | "ID=" `isInfixOf` s =
          case dropWhile (/= '=') (dropWhile (/= 'I') s) of
            ('=':resto) -> Just (takeWhile (\c -> c /= ' ' && c /= ')') resto)
            _ -> Nothing
      | otherwise = Nothing
    
    -- encontra o indice de uma palavra em uma lista de palavras
    encontrarIndice :: String -> [String] -> Int
    encontrarIndice _ [] = -1
    encontrarIndice palavra (x:xs)
      | x == palavra = 0
      | otherwise = let resto = encontrarIndice palavra xs
                    in if resto < 0 then -1 else resto + 1
