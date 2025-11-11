{-# LANGUAGE OverloadedStrings #-}

module Main where

import InventarioDados
import qualified InventarioLogica as Logic

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hFlush, stdout)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException, evaluate)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Control.Monad (when)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaFile :: FilePath
auditoriaFile = "Auditoria.log"




-- funcao para carregar inventario do disco capturando excecoes e lidando com elas para evitar falhas
loadInventario :: IO Inventario
loadInventario = do
  conteudo <- catch (readFile inventarioFile) handler
  case readMaybe conteudo :: Maybe Inventario of
    Just inv -> return inv
    Nothing  -> return Map.empty
  where
    handler :: IOException -> IO String
    handler e
      | isDoesNotExistError e = return "empty"
      | otherwise             = return "empty"
      
      
      


-- funcao para carregar os logs de auditoria lidando com arquivos vazios e ignorando linhas corrompidas. Separa os dados por linha
loadLogs :: IO [LogEntry]
loadLogs = do
  conteudo <- catch (readFile auditoriaFile) (\e -> if isDoesNotExistError e then return "" else return "")
  _ <- evaluate (length conteudo)
  let ls = lines conteudo
  return $ catMaybes $ map (\l -> readMaybe l :: Maybe LogEntry) ls




-- adiciona no final do ficheiro e usa show para serializacao
appendLog :: LogEntry -> IO ()
appendLog le = appendFile auditoriaFile (show le ++ "\n")


-- pega o inventario(map) transforma em texto e grava no arquivo Inventario.dat
saveInventario :: Inventario -> IO ()
saveInventario inv = writeFile inventarioFile (show inv)



-- exibe p sem salto de linha e le a linha do usuario, serve para perguntas e respostas
prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine



-- processamento dos comandos
processCommand :: Inventario -> [LogEntry] -> String -> IO Inventario
processCommand inv _lineLogs input =
  case words input of
    [] -> putStrLn "" >> return inv
    ("ajuda":_) -> do
      putStrLn textoAjuda
      return inv

    ("sair":_) -> do
      putStrLn "Saindo..."
      return inv
    
    ("listar":_) -> do
      putStrLn "Inventário atual:"
      if Map.null inv
        then putStrLn "(vazio)"
        else mapM_ (printItem . snd) (Map.toList inv)
      return inv

    ("adicionar":_) -> do
      now <- getCurrentTime
      iID <- prompt "ID do item: "
      nm  <- prompt "Nome: "
      qtdS <- prompt "Quantidade (Int): "
      catg <- prompt "Categoria: "
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

    ("remover":xs) -> case xs of
      (iID:qtdS:_) ->
        case readMaybe qtdS :: Maybe Int of
          Nothing -> do
            putStrLn "Quantidade inválida. Uso: remover <itemID> [quantidade]"
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
      (iID:_) -> do
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
    
      _ -> putStrLn "Uso: remover <itemID> [quantidade]" >> return inv

      

    ("atualizar":xs) -> case xs of
      (iID:qtdS:_) -> do
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
      _ -> putStrLn "Uso: atualizar <itemID> <novaQuantidade>" >> return inv


      
    ("report":_) -> do
      logs <- loadLogs
      putStrLn "=== Relatório básico ==="
      putStrLn $ "Total de entradas de log: " ++ show (length logs)
      let erros = logsErro logs
      putStrLn $ "Entradas de erro: " ++ show (length erros)
      mapM_ printLogEntry erros
      return inv

    _ -> putStrLn "Comando desconhecido. Digite 'ajuda' para ver comandos." >> return inv
    


-- formata uma linha simples em um item usando os getters itemID, nome, quantidade e categoria
printItem :: Item -> IO ()
printItem it =
  putStrLn $ itemID it ++ " | " ++ nome it ++ " | qtd: " ++ show (quantidade it) ++ " | cat: " ++ categoria it



-- formata um log entry para exibir timestamp, acaoo, detalhes e status
printLogEntry :: LogEntry -> IO ()
printLogEntry le = putStrLn $ show (timestamp le) ++ " - " ++ show (acao le) ++ " - " ++ detalhes le ++ " - " ++ show (status le)



-- funcao que filtra log entry que status é Falha _
logsErro :: [LogEntry] -> [LogEntry]
logsErro = filter isErro
  where
    isErro (LogEntry _ _ _ (Falha _)) = True
    isErro _                          = False




-- Texto de ajuda
textoAjuda :: String
textoAjuda = unlines
  [ "Comandos disponíveis:"
  , "  Ajuda                      - mostra esta ajuda"
  , "  adicionar                  - adiciona um item (interativo)"
  , "  remover <itemID>           - remove item (totalmente)"
  , "  atualizar <itemID> <q>     - atualiza quantidade para q (Int)"
  , "  listar                     - lista inventário em memória"
  , "  relatorio                  - relatório básico de logs (erros)"
  , "  sair                       - encerra o programa"
  ]


-- loop principal
-- repl é um loop recursivo que mantem o estado do inventario e le os comandos repetidas vezes
repl :: Inventario -> [LogEntry] -> IO ()
repl inv logs = do
  line <- prompt "inventario> "
  let trimmed = dropWhile (== ' ') line
  if trimmed == "sair"
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
  putStrLn "Digite 'ajuda' para ver comandos."
  repl inv logs
