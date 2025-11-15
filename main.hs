{-# LANGUAGE OverloadedStrings #-}

module Main where

import InventarioDados
import qualified InventarioLogica as Logic
import qualified InventarioAnalise as Analise

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hFlush, stdout)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException, evaluate)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Monad (when)
import Data.Char (isSpace)  -- (adição) utilitário para sanitização

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaFile :: FilePath
auditoriaFile = "Auditoria.log"

-- utilitários de sanitização
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

collapseSpaces :: String -> String
collapseSpaces = unwords . words

sanitizeText :: String -> String
sanitizeText = collapseSpaces . trim

sanitizeId :: String -> String
sanitizeId = filter (not . isSpace)

-- função para carregar inventário do disco capturando exceções e lidando com elas para evitar falhas
loadInventario :: IO Inventario
loadInventario = do
  conteudo <- catch (readFile inventarioFile) handler
  case readMaybe conteudo :: Maybe Inventario of
    Just inv -> return inv
    Nothing -> return Map.empty
  where
    handler :: IOException -> IO String
    handler e
      | isDoesNotExistError e = return "empty"
      | otherwise = return "empty"

-- função para carregar os logs de auditoria lidando com arquivos vazios e ignorando linhas corrompidas.
loadLogs :: IO [LogEntry]
loadLogs = do
  conteudo <- catch (readFile auditoriaFile) (\e -> if isDoesNotExistError e then return "" else return "")
  _ <- evaluate (length conteudo)
  let ls = lines conteudo
  return $ catMaybes $ map (\l -> readMaybe l :: Maybe LogEntry) ls

-- adiciona no final do ficheiro e usa show para serialização
appendLog :: LogEntry -> IO ()
appendLog le = appendFile auditoriaFile (show le ++ "\n")

-- pega o inventário (map) transforma em texto e grava no arquivo Inventario.dat
saveInventario :: Inventario -> IO ()
saveInventario inv = writeFile inventarioFile (show inv)

-- exibe prompt sem salto de linha e lê a linha do usuário, serve para perguntas e respostas
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

-- processamento dos comandos
processCommand :: Inventario -> [LogEntry] -> String -> IO Inventario
processCommand inv _lineLogs input =
  case words input of
    [] -> putStrLn "" >> return inv
    ("help":_) -> do
      putStrLn textoAjuda
      return inv

    ("exit":_) -> do
      putStrLn "Saindo..."
      return inv

    ("list":_) -> do
      putStrLn "Inventário atual:"
      if Map.null inv
        then putStrLn "(vazio)"
        else mapM_ (printItem . snd) (Map.toList inv)
      return inv

    ("add":_) -> do
      now <- getCurrentTime
      iIDraw <- prompt "ID do item: "
      nmRaw  <- prompt "Nome: "
      qtdS   <- prompt "Quantidade: "
      catRaw <- prompt "Categoria: "
      let iID = sanitizeId   iIDraw
          nm  = sanitizeText nmRaw
          catg = sanitizeText catRaw
      case readMaybe qtdS :: Maybe Int of
        Nothing -> do
          putStrLn "Quantidade inválida."
          let logE = LogEntry now Add ("Tentativa de adicionar com quantidade inválida: " ++ qtdS) (Falha "Quantidade inválida")
          appendLog logE
          return inv
        Just qtd -> do
          let item = Item { itemID = iID, nome = nm, quantidade = qtd, categoria = catg }
          case Logic.addItem now item inv of
            Left err -> do
              putStrLn $ "Falha: " ++ err
              let logE = LogEntry now Add ("Falha ao adicionar ID=" ++ iID) (Falha err)
              appendLog logE
              return inv
            Right (novoInv, logE) -> do
              saveInventario novoInv
              appendLog logE
              putStrLn "Item adicionado com sucesso."
              return novoInv

    ("remove":xs) -> case xs of
      (iIDraw:qtdS:_) ->
        let iID = sanitizeId iIDraw in
        case readMaybe qtdS :: Maybe Int of
          Nothing -> do
            putStrLn "Quantidade inválida. Uso: remove <itemID> [quantidade]"
            now <- getCurrentTime
            appendLog (LogEntry now Remove
                         ("Tentativa de remover com quantidade inválida: " ++ qtdS ++ " para ID=" ++ iID)
                         (Falha "Quantidade inválida"))
            return inv
          Just qtd -> do
            now <- getCurrentTime
            case Logic.removeItem now iID qtd inv of
              Left err -> do
                putStrLn $ "Falha: " ++ err
                let logE = LogEntry now Remove ("Falha ao remover ID=" ++ iID ++ " qtd=" ++ show qtd) (Falha err)
                appendLog logE
                return inv
              Right (novoInv, logE) -> do
                saveInventario novoInv
                appendLog logE
                putStrLn "Item removido com sucesso."
                return novoInv

      -- remove tudo
      (iIDraw:_) -> do
        let iID = sanitizeId iIDraw
        now <- getCurrentTime
        case Map.lookup iID inv of
          Nothing -> do
            putStrLn "Falha: item não encontrado."
            let logE = LogEntry now Remove ("Falha ao remover ID=" ++ iID ++ " (não encontrado)") (Falha "Item não encontrado")
            appendLog logE
            return inv
          Just it -> do
            let qtd = quantidade it
            case Logic.removeItem now iID qtd inv of
              Left err -> do
                putStrLn $ "Falha: " ++ err
                let logE = LogEntry now Remove ("Falha ao remover ID=" ++ iID ++ " qtd=" ++ show qtd) (Falha err)
                appendLog logE
                return inv
              Right (novoInv, logE) -> do
                saveInventario novoInv
                appendLog logE
                putStrLn $ "Item removido (" ++ show qtd ++ " unidades)."
                return novoInv
      _ -> putStrLn "Uso: remove <itemID> [quantidade]" >> return inv

    ("update":xs) -> case xs of
      (iIDraw:qtdS:_) -> do
        let iID = sanitizeId iIDraw
        now <- getCurrentTime
        case readMaybe qtdS :: Maybe Int of
          Nothing -> do
            putStrLn "Quantidade inválida."
            let logE = LogEntry now Update ("Tentativa de update com quantidade inválida: " ++ qtdS) (Falha "Quantidade inválida")
            appendLog logE
            return inv
          Just novaQtd ->
            case Logic.updateQty now iID novaQtd inv of
              Left err -> do
                putStrLn $ "Falha: " ++ err
                let logE = LogEntry now Update ("Falha ao atualizar ID=" ++ iID) (Falha err)
                appendLog logE
                return inv
              Right (novoInv, logE) -> do
                saveInventario novoInv
                appendLog logE
                putStrLn "Quantidade atualizada com sucesso."
                return novoInv
      _ -> putStrLn "Uso: update <itemID> <novaQuantidade>" >> return inv

    ("report":_) -> do
      logs <- loadLogs
      putStrLn "\n=========================================="
      putStrLn "===     RELATÓRIO DE AUDITORIA         ==="
      putStrLn "==========================================\n"

      putStrLn $ "Total de entradas de log: " ++ show (length logs)

      let erros = Analise.logsDeErro logs
      putStrLn $ "\nTotal de operações com erro: " ++ show (length erros)

      when (not $ null erros) $ do
        putStrLn "\n--- Detalhes dos Erros ---"
        mapM_ printLogEntry erros

      let historico = Analise.historicoPorItem logs
      putStrLn $ "\n\nTotal de itens com movimentação: " ++ show (Map.size historico)

      when (Map.size historico > 0) $ do
        putStrLn "\n--- Histórico de Operações por Item ---"
        mapM_ (printHistoricoItem) (Map.toList historico)

      case Analise.itemMaisMovimentado logs of
        Nothing -> putStrLn "\nNenhum item movimentado ainda."
        Just (itemId, numOps) -> do
          putStrLn $ "\n--- Item Mais Movimentado ---"
          putStrLn $ "Item ID: " ++ itemId
          putStrLn $ "Número de operações: " ++ show numOps

      putStrLn "\n==========================================\n"
      return inv

    _ -> putStrLn "Comando desconhecido. Digite 'help' para ver comandos." >> return inv

-- formata uma linha simples em um item usando os getters itemID, nome, quantidade e categoria
printItem :: Item -> IO ()
printItem it = putStrLn $ itemID it ++ " | " ++ nome it ++ " | qtd: " ++ show (quantidade it) ++ " | cat: " ++ categoria it

-- formata um log entry para exibir timestamp, ação, detalhes e status
printLogEntry :: LogEntry -> IO ()
printLogEntry le = putStrLn $ show (timestamp le) ++ " - " ++ show (acao le) ++ " - " ++ detalhes le ++ " - " ++ show (status le)

-- exibe histórico de um item específico
printHistoricoItem :: (String, [LogEntry]) -> IO ()
printHistoricoItem (itemId, entries) = do
  putStrLn $ "\nItem: " ++ itemId ++ " (" ++ show (length entries) ++ " operações)"
  mapM_ (\le -> putStrLn $ "  - " ++ show (acao le) ++ ": " ++ detalhes le ++ " [" ++ show (status le) ++ "]") entries

-- Texto de ajuda
textoAjuda :: String
textoAjuda = unlines
  [ "Comandos disponíveis:"
  , "  help                       - mostra os comandos disponíveis"
  , "  add                        - adiciona um item"
  , "  remove <itemID> <qtd>      - remove quantidade e, caso seja omitida a quantidade, remove a totalidade de itens"
  , "  update <itemID> <q>        - atualiza quantidade"
  , "  list                       - lista inventário em memória"
  , "  report                     - relatório completo de logs e análises"
  , "  exit                       - encerra o programa"
  ]

-- repl é um loop recursivo que mantém o estado do inventário e lê os comandos repetidas vezes
repl :: Inventario -> [LogEntry] -> IO ()
repl inv logs = do
  line <- prompt "inventario> "
  let trimmed = dropWhile (== ' ') line
  if trimmed == "exit"
    then putStrLn "Encerrando..." >> return ()
    else do
      novoInv <- processCommand inv logs trimmed
      novosLogs <- loadLogs
      repl novoInv novosLogs

main :: IO ()
main = do
  putStrLn "Inicializando sistema de inventário..."
  inv <- loadInventario
  logs <- loadLogs
  putStrLn $ "Inventário carregado: " ++ show (Map.size inv) ++ " itens."
  putStrLn $ "Logs carregados: " ++ show (length logs)
  putStrLn "Digite 'help' para ver comandos."
  repl inv logs
