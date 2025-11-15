## Sistema de Inventario

**Instituição:** Pontifícia Universidade Católica do Paraná 

**Disciplina:** Programação Lógica e Funcional

**Professor:** Frank Coelho de Alcantara 

**Grupo: 8 - Toda Disciplina**

**Alunos:**
- Danillo Gonçalves Camargo da Silva - [danltargz](https://github.com/danltargz) - Aluno 4
- Guilherme Felippe Lazari - [Guilherme006](https://github.com/Guilherme006) - Aluno 1
- Rodrigo Schiavinatto Plassmann - [rodrigoplassmann](https://github.com/rodrigoplassmann) - Aluno 2
- Thomas Manussadjian Steinhausser - [DraNefario](https://github.com/DraNefario) - Aluno 3

**Link do GDB online:** [onlinegdb.com](https://onlinegdb.com/1yn1mQRTMc)

---

## Arquitetura do Sistema

O sistema é composto por quatro módulos principais, cada um com responsabilidades bem definidas seguindo princípios de programação funcional.

---

### InventarioDados.hs - Tipos de Dados

Este módulo contém todas as definições de tipos do domínio do problema. Todos os tipos derivam `Show` e `Read` para permitir serialização automática.

#### **Item: Representação de um Item do Inventário**

```haskell
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  }
  deriving (Show, Read, Eq)
```

**Explicação:**
- `Item` é um tipo de registro (record) com quatro campos
- `itemID`: identificador único do item (String)
- `nome`: nome descritivo do item
- `quantidade`: número de unidades em estoque (sempre >= 0)
- `categoria`: classificação do item (ex: "Perifericos", "Componentes")
- `deriving (Show, Read, Eq)`: permite conversão automática de String e comparação de igualdade

---

#### **Inventario: Estrutura de Dados Principal**

```haskell
type Inventario = Map String Item
```

**Explicação:**
- `Inventario` é um sinônimo de tipo (type alias) para `Map String Item`
- Usa `Data.Map` do módulo padrão de Haskell para busca eficiente (O(log n))
- A chave é o `itemID` (String) e o valor é o `Item` completo
- Permite acesso rápido a qualquer item pelo seu ID

---

#### **AcaoLog: Tipo Algébrico para Ações**

```haskell
data AcaoLog
  = Add
  | Remove
  | Update
  | QueryFail
  | List
  | Report
  deriving (Show, Read, Eq)
```

**Explicação:**
- **ADT (Algebraic Data Type)** que representa todas as ações possíveis no sistema
- `Add`: adição de novo item
- `Remove`: remoção de quantidade de item
- `Update`: atualização de quantidade
- `QueryFail`: falha em operação de consulta
- `List`: listagem do inventário
- `Report`: geração de relatório
- Cada construtor é um valor do tipo `AcaoLog`

---

#### **StatusLog: Resultado de uma Operação**

```haskell
data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)
```

**Explicação:**
- **ADT** que representa o resultado de uma operação
- `Sucesso`: operação completada com êxito
- `Falha String`: operação falhou, com mensagem de erro descritiva
- Similar ao tipo `Either`, mas específico para logs
- O construtor `Falha` carrega uma `String` com detalhes do erro

---

#### **LogEntry: Registro Completo de Log**

```haskell
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  }
  deriving (Show, Read, Eq)
```

**Explicação:**
- Estrutura completa de uma entrada no log de auditoria
- `timestamp`: momento exato da operação (UTC)
- `acao`: qual operação foi realizada (tipo `AcaoLog`)
- `detalhes`: descrição textual da operação
- `status`: resultado da operação (Sucesso ou Falha)
- Cada operação no sistema gera exatamente um `LogEntry`

**Exemplo de LogEntry:**
```haskell
LogEntry {timestamp = 2025-11-14 10:30:45.123 UTC, acao = Add, detalhes = "Item adicionado: Teclado Mecanico (ID: 1)", status = Sucesso}
```

---

### InventarioLogica.hs - Lógica de Negócio Pura

Este módulo contém todas as regras de negócio do sistema. **Importante**: nenhuma função aqui realiza I/O - todas são puras.

#### **ResultadoOperacao: Tipo para Retorno das Funções**

```haskell
type ResultadoOperacao = (Inventario, LogEntry)
```

**Explicação:**
- Sinônimo de tipo para simplificar assinaturas
- Representa o resultado de uma operação bem-sucedida
- Tupla contendo:
  1. Novo estado do inventário (após a operação)
  2. Entrada de log que documenta a operação

---

#### **addItem: Adicionar Item ao Inventário**

```haskell
addItem :: UTCTime -> Item -> Inventario -> Either String ResultadoOperacao
addItem tempo item inv
  | quantidade item < 0 =
      Left "Erro: quantidade inicial invalida (menor que zero)."
  | Map.member (itemID item) inv =
      Left "Erro: item ja existe no inventario."
  | otherwise =
      let novoInv  = Map.insert (itemID item) item inv
          logEntry = LogEntry tempo Add
                        ("Item adicionado: " ++ nome item ++ " (ID: " ++ itemID item ++ ")")
                        Sucesso
      in Right (novoInv, logEntry)
```

**Assinatura:**
- `UTCTime`: timestamp da operação
- `Item`: item a ser adicionado
- `Inventario`: estado atual do inventário
- Retorna: `Either String ResultadoOperacao`
  - `Left String`: erro com mensagem descritiva
  - `Right (Inventario, LogEntry)`: sucesso com novo estado e log

**Validações (guards):**
1. `quantidade item < 0`: rejeita quantidades negativas
2. `Map.member (itemID item) inv`: verifica se ID já existe (evita duplicatas)
3. `otherwise`: se passou nas validações, procede

**Lógica de sucesso:**
- `novoInv = Map.insert ...`: cria novo Map com o item inserido (imutável!)
- `logEntry = LogEntry ...`: cria registro de log documentando a operação
- `Right (novoInv, logEntry)`: retorna ambos encapsulados em `Right`

**Exemplo de uso:**
```haskell
let item = Item "1" "Teclado" 15 "Perifericos"
    inv = Map.empty
    tempo = getCurrentTime  -- (em contexto IO)
in case addItem tempo item inv of
     Left err -> putStrLn $ "Erro: " ++ err
     Right (novoInv, log) -> -- sucesso!
```

---

#### **removeItem: Remover Quantidade de Item**

```haskell
removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem tempo idItem qtd inv
  | qtd <= 0 = Left "Erro: quantidade a remover deve ser positiva."
  | otherwise =
      case Map.lookup idItem inv of
        Nothing -> Left "Erro: item nao encontrado no inventario."
        Just it ->
          let atual = quantidade it
          in if qtd > atual
               then Left "Erro: estoque insuficiente."
               else
                 let it'    = it { quantidade = atual - qtd }
                     inv'   = Map.insert idItem it' inv
                     logEnt = LogEntry tempo Remove
                               ("Removidas " ++ show qtd ++ " unidades do item " ++ idItem)
                               Sucesso
                 in Right (inv', logEnt)
```

**Explicação detalhada:**

**Assinatura:**
- `String`: ID do item a remover
- `Int`: quantidade a remover
- Demais parâmetros iguais a `addItem`

**Validações:**
1. `qtd <= 0`: rejeita quantidades não-positivas
2. `Map.lookup idItem inv`: verifica se o item existe
3. `qtd > atual`: verifica se há estoque suficiente (**regra de negócio crítica!**)

**Lógica de sucesso:**
- `it' = it { quantidade = atual - qtd }`: **record update syntax** - cria novo Item com quantidade atualizada
- `inv' = Map.insert idItem it' inv`: substitui o item antigo pelo novo no Map
- Cria log de sucesso documentando quantas unidades foram removidas

**Importante:**
- O item permanece no inventário mesmo se a quantidade chegar a 0
- Para remover completamente, seria necessária outra função (ex: `deleteItem`)

---

#### **updateQty: Atualizar Quantidade de Item**

```haskell
updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty tempo idItem novaQtd inv
  | novaQtd < 0 =
      Left "Erro: quantidade invalida (menor que zero)."
  | otherwise =
      case Map.lookup idItem inv of
        Nothing ->
          Left "Erro: item nao encontrado no inventario."
        Just itemAntigo ->
          let itemAtualizado = itemAntigo { quantidade = novaQtd }
              novoInv = Map.insert idItem itemAtualizado inv
              logEntry = LogEntry tempo Update
                ("Atualizada quantidade do item " ++ idItem ++ " para " ++ show novaQtd)
                Sucesso
          in Right (novoInv, logEntry)
```

**Explicação detalhada:**

**Diferença entre `removeItem` e `updateQty`:**
- `removeItem`: **decrementa** a quantidade existente (`atual - qtd`)
- `updateQty`: **define** uma nova quantidade absoluta (`novaQtd`)

**Validações:**
1. `novaQtd < 0`: rejeita quantidades negativas
2. Verifica se o item existe no inventário

**Lógica:**
- `itemAtualizado = itemAntigo { quantidade = novaQtd }`: substitui completamente a quantidade
- Útil para correções de inventário ou ajustes após contagem física

**Exemplo de uso:**
```haskell
-- Teclado tinha 15 unidades, queremos ajustar para 20
updateQty tempo "1" 20 inv
-- Resultado: Teclado agora tem 20 unidades (não 35!)
```

---

### InventarioAnalise.hs - Análise de Logs

Este módulo contém funções puras para processar e analisar os logs de auditoria, gerando relatórios.

#### **logsDeErro: Filtrar Logs com Falhas**

```haskell
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro (LogEntry _ _ _ (Falha _)) = True
    isErro _ = False
```

**Explicação detalhada:**

**Função auxiliar `isErro`:**
- Pattern matching no quarto campo (status) do `LogEntry`
- `(Falha _)`: casa com qualquer falha (ignora a mensagem com `_`)
- `_ = False`: todos os outros casos (incluindo `Sucesso`)

**Uso da função `filter`:**
- `filter :: (a -> Bool) -> [a] -> [a]`
- Aplica `isErro` a cada elemento da lista
- Retorna apenas os elementos onde `isErro` retorna `True`

**Exemplo:**
```haskell
logs = [ LogEntry ... Sucesso
       , LogEntry ... (Falha "Erro: estoque insuficiente")
       , LogEntry ... Sucesso
       ]
       
logsDeErro logs 
-- Retorna: [LogEntry ... (Falha "Erro: estoque insuficiente")]
```

---

#### **historicoPorItem: Agrupar Operações por Item**

```haskell
historicoPorItem :: [LogEntry] -> Map String [LogEntry]
historicoPorItem logs = 
  let logsComID = [(extrairItemID le, le) | le <- logs]
      logsValidos = [(itemId, logEntry) | (Just itemId, logEntry) <- logsComID]
      agrupados = Map.fromListWith (++) [(itemId, [logEntry]) | (itemId, logEntry) <- logsValidos]
  in agrupados
```

**Explicação detalhada:**

**Passo 1 - `logsComID`:**
```haskell
[(extrairItemID le, le) | le <- logs]
-- List comprehension que gera: [(Maybe String, LogEntry)]
```
- Para cada `LogEntry`, tenta extrair o `itemID` usando `extrairItemID`
- Resultado: lista de tuplas `(Maybe String, LogEntry)`
- Pode ser `(Just "1", logEntry)` ou `(Nothing, logEntry)`

**Passo 2 - `logsValidos`:**
```haskell
[(itemId, logEntry) | (Just itemId, logEntry) <- logsComID]
```
- **Pattern matching** filtra apenas as tuplas com `Just itemId`
- Descarta logs onde não foi possível extrair o ID (ex: comandos `list`, `report`)
- Resultado: `[(String, LogEntry)]`

**Passo 3 - `agrupados`:**
```haskell
Map.fromListWith (++) [(itemId, [logEntry]) | (itemId, logEntry) <- logsValidos]
```
- `Map.fromListWith :: (a -> a -> a) -> [(k, a)] -> Map k a`
- Quando encontra chaves duplicadas, aplica a função `(++)` para combinar os valores
- Cada `logEntry` é colocado em uma lista singleton `[logEntry]`
- Listas de mesmo `itemId` são concatenadas com `++`

**Resultado final:**
```haskell
Map.fromList 
  [ ("1", [log1_item1, log2_item1, log3_item1])
  , ("2", [log1_item2])
  , ("TECLADO", [log1_teclado, log2_teclado])
  ]
```

**Função auxiliar `extrairItemID`:**
```haskell
extrairItemID :: LogEntry -> Maybe String
extrairItemID (LogEntry _ acao det _) = 
  case acao of
    Add -> extrairDeAdd det
    Remove -> extrairDeRemove det
    Update -> extrairDeUpdate det
    _ -> Nothing
```
- Analisa o campo `detalhes` do log para extrair o ID do item
- Diferentes padrões para diferentes ações:
  - **Add**: procura por `(ID: XXX)` no texto
  - **Remove**: procura por `do item XXX` ou `ID=XXX`
  - **Update**: mesma lógica que Remove
- Retorna `Nothing` para logs que não se referem a itens específicos

---

#### **itemMaisMovimentado: Identificar Item com Mais Operações**

```haskell
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  let historico = historicoPorItem logs
      contagens = Map.toList $ Map.map length historico
  in case contagens of
       [] -> Nothing
       _ -> Just $ maximoPor snd contagens
  where
    maximoPor f xs = foldl1 (\a b -> if f a >= f b then a else b) xs
```

**Explicação detalhada:**

**Passo 1 - Obter histórico:**
```haskell
historico = historicoPorItem logs
-- Map String [LogEntry]
```

**Passo 2 - Calcular contagens:**
```haskell
contagens = Map.toList $ Map.map length historico
-- Map.map length: transforma Map String [LogEntry] -> Map String Int
-- Map.toList: converte Map -> [(String, Int)]
```
- Exemplo: `[("1", 1), ("TECLADO", 2), ("2", 1)]`

**Passo 3 - Encontrar máximo:**
```haskell
case contagens of
  [] -> Nothing  -- Nenhum log encontrado
  _ -> Just $ maximoPor snd contagens
```

**Função auxiliar `maximoPor`:**
```haskell
maximoPor f xs = foldl1 (\a b -> if f a >= f b then a else b) xs
```
- `f`: função para extrair valor de comparação (no caso, `snd` pega o segundo elemento da tupla)
- `foldl1`: fold que usa o primeiro elemento como acumulador inicial
- Compara pares de elementos e mantém o maior

**Resultado:**
```haskell
Just ("TECLADO", 2)  -- TECLADO tem 2 operações (mais que qualquer outro)
```

---

### main.hs - I/O e Persistência

Este módulo é responsável por toda a interação com o mundo externo (I/O): arquivos, terminal, timestamps.

#### **Constantes e Utilitários**

```haskell
inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaFile :: FilePath
auditoriaFile = "Auditoria.log"
```
- Define os nomes dos arquivos usados para persistência
- Centralizados para facilitar modificação

**Funções de sanitização:**
```haskell
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
```
- Remove espaços no início e fim de strings
- `f` é aplicado duas vezes: `reverse . dropWhile` remove do fim, depois inverte e remove do início

```haskell
sanitizeText :: String -> String
sanitizeText = collapseSpaces . trim

sanitizeId :: String -> String
sanitizeId = filter (not . isSpace)
```
- `sanitizeText`: normaliza textos (nomes, categorias) removendo espaços extras
- `sanitizeId`: remove TODOS os espaços de IDs (IDs não devem ter espaços)

---

#### **loadInventario: Carregar Estado do Disco**

```haskell
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
```

**Explicação detalhada:**

**Tratamento de exceções:**
```haskell
catch (readFile inventarioFile) handler
```
- `catch :: IO a -> (Exception -> IO a) -> IO a`
- Tenta ler o arquivo; se der erro, chama `handler`

**Handler:**
```haskell
handler e
  | isDoesNotExistError e = return "empty"
  | otherwise = return "empty"
```
- Se arquivo não existe: retorna "empty" (string inválida para forçar `readMaybe` falhar)
- Qualquer outro erro: também retorna "empty"
- **Não trava o programa!**

**Desserialização:**
```haskell
case readMaybe conteudo :: Maybe Inventario of
  Just inv -> return inv
  Nothing -> return Map.empty
```
- `readMaybe :: Read a => String -> Maybe a`
- Tenta converter String em `Inventario` (possível porque `Map` deriva `Read`)
- Se falhar: retorna inventário vazio (`Map.empty`)
- **Primeira execução**: arquivo não existe → retorna inventário vazio

---

#### **loadLogs: Carregar Logs do Disco**

```haskell
loadLogs :: IO [LogEntry]
loadLogs = do
  conteudo <- catch (readFile auditoriaFile) 
                    (\e -> if isDoesNotExistError e then return "" else return "")
  _ <- evaluate (length conteudo)  -- Força leitura completa do arquivo
  let ls = lines conteudo
  return $ catMaybes $ map (\l -> readMaybe l :: Maybe LogEntry) ls
```

**Explicação detalhada:**

**Forçar leitura completa:**
```haskell
_ <- evaluate (length conteudo)
```
- `evaluate :: a -> IO a`
- Força a avaliação da string **imediatamente**
- Sem isso, Haskell lazy poderia causar problemas ao fechar o arquivo

**Processar linhas:**
```haskell
let ls = lines conteudo
return $ catMaybes $ map (\l -> readMaybe l :: Maybe LogEntry) ls
```
1. `lines`: divide string em lista de linhas
2. `map (\l -> readMaybe l)`: tenta converter cada linha em `LogEntry`
   - Resultado: `[Maybe LogEntry]`
3. `catMaybes`: remove todos os `Nothing`, mantém apenas os `Just`
   - `[Maybe a] -> [a]`
   
**Robustez:**
- Linhas corrompidas são silenciosamente ignoradas
- Sistema continua funcionando mesmo se log estiver parcialmente danificado

---

#### **appendLog e saveInventario: Persistir Dados**

```haskell
appendLog :: LogEntry -> IO ()
appendLog le = appendFile auditoriaFile (show le ++ "\n")
```
**Explicação:**
- `appendFile :: FilePath -> String -> IO ()`
- **Modo append**: adiciona ao final do arquivo sem apagar conteúdo anterior
- `show le`: serializa `LogEntry` para String
- Cada log é uma linha (termina com `\n`)

```haskell
saveInventario :: Inventario -> IO ()
saveInventario inv = writeFile inventarioFile (show inv)
```
**Explicação:**
- `writeFile :: FilePath -> String -> IO ()`
- **Sobrescreve** o arquivo completamente (modo write)
- Serializa todo o `Map` de uma vez

**Padrão de uso no sistema:**
1. Operação de sucesso: `saveInventario novoInv` **e** `appendLog logE`
2. Operação de falha: **apenas** `appendLog logE` (não salva inventário)

---

#### **processCommand: Parser e Executor de Comandos**

Esta é uma das funções mais complexas. Vou explicar comando por comando.

**Estrutura geral:**
```haskell
processCommand :: Inventario -> [LogEntry] -> String -> IO Inventario
processCommand inv _lineLogs input =
  case words input of
    [] -> putStrLn "" >> return inv
    ("help":_) -> ...
    ("add":_) -> ...
    -- etc
```
- `words input`: divide input em palavras
- Pattern matching no primeiro elemento da lista (comando)
- Retorna `IO Inventario`: o novo estado do inventário após o comando

---

**Comando `add`:**
```haskell
("add":_) -> do
  now <- getCurrentTime
  iIDraw <- prompt "ID do item: "
  nmRaw <- prompt "Nome: "
  qtdS <- prompt "Quantidade: "
  catRaw <- prompt "Categoria: "
  let iID = sanitizeId iIDraw
      nm = sanitizeText nmRaw
      catg = sanitizeText catRaw
  case readMaybe qtdS :: Maybe Int of
    Nothing -> do
      putStrLn "Quantidade invalida."
      let logE = LogEntry now Add (...) (Falha "Quantidade invalida")
      appendLog logE
      return inv  -- Retorna inventário INALTERADO
    Just qtd -> do
      let item = Item { itemID = iID, nome = nm, quantidade = qtd, categoria = catg }
      case Logic.addItem now item inv of
        Left err -> do
          putStrLn $ "Falha: " ++ err
          let logE = LogEntry now Add (...) (Falha err)
          appendLog logE
          return inv  -- Retorna inventário INALTERADO
        Right (novoInv, logE) -> do
          saveInventario novoInv    -- PERSISTE novo estado
          appendLog logE            -- REGISTRA no log
          putStrLn "Item adicionado com sucesso."
          return novoInv            -- Retorna NOVO inventário
```

**Fluxo:**
1. Obtém timestamp atual com `getCurrentTime :: IO UTCTime`
2. Faz perguntas interativas ao usuário
3. Sanitiza entradas (remove espaços, normaliza)
4. Valida quantidade (conversão para `Int`)
5. Chama função pura `Logic.addItem`
6. Trata resultado com pattern matching em `Either`:
   - `Left err`: falha → registra log, retorna inventário inalterado
   - `Right (novoInv, logE)`: sucesso → salva novo estado, registra log, retorna novo inventário

---

**Comando `remove`:**
```haskell
("remove":xs) -> case xs of
  (iIDraw:qtdS:_) -> -- remove quantidade específica
    ...
  (iIDraw:_) -> -- remove totalidade (assume quantidade = quantidade atual)
    ...
  _ -> putStrLn "Uso: remove <itemID> [quantidade]" >> return inv
```

**Explicação da sintaxe:**
- `("remove":xs)`: captura palavra "remove" e **resto** dos argumentos em `xs`
- Segundo `case xs of`: verifica quantos argumentos foram passados
  - 2+ argumentos: `(iIDraw:qtdS:_)` → remove quantidade específica
  - 1 argumento: `(iIDraw:_)` → remove totalidade
  - 0 argumentos: `_` → mostra mensagem de uso

**Lógica "remove totalidade":**
```haskell
(iIDraw:_) -> do
  let iID = sanitizeId iIDraw
  now <- getCurrentTime
  case Map.lookup iID inv of
    Nothing -> -- item não encontrado, registra erro
    Just it -> do
      let qtd = quantidade it  -- Obtém quantidade atual
      case Logic.removeItem now iID qtd inv of  -- Remove TUDO
        Left err -> ...
        Right (novoInv, logE) -> ...
```

---

**Comando `report`:**
```haskell
("report":_) -> do
  logs <- loadLogs  -- Recarrega logs do disco
  putStrLn "\n=========================================="
  putStrLn "===     RELATORIO DE AUDITORIA         ==="
  putStrLn "==========================================\n"

  putStrLn $ "Total de entradas de log: " ++ show (length logs)

  let erros = Analise.logsDeErro logs
  putStrLn $ "\nTotal de operacoes com erro: " ++ show (length erros)

  when (not $ null erros) $ do
    putStrLn "\n--- Detalhes dos Erros ---"
    mapM_ printLogEntry erros

  let historico = Analise.historicoPorItem logs
  putStrLn $ "\n\nTotal de itens com movimentacao: " ++ show (Map.size historico)

  when (Map.size historico > 0) $ do
    putStrLn "\n--- Historico de Operacoes por Item ---"
    mapM_ printHistoricoItem (Map.toList historico)

  case Analise.itemMaisMovimentado logs of
    Nothing -> putStrLn "\nNenhum item movimentado ainda."
    Just (itemId, numOps) -> do
      putStrLn $ "\n--- Item Mais Movimentado ---"
      putStrLn $ "Item ID: " ++ itemId
      putStrLn $ "Numero de operacoes: " ++ show numOps

  putStrLn "\n==========================================\n"
  return inv
```

**Explicação detalhada:**

**Estrutura do relatório:**
1. **Estatísticas gerais**: total de logs
2. **Análise de erros**: usa `logsDeErro` para filtrar e exibir falhas
3. **Histórico por item**: usa `historicoPorItem` para agrupar operações
4. **Item mais movimentado**: usa `itemMaisMovimentado` para ranking

**Uso de `when`:**
```haskell
when (not $ null erros) $ do
  putStrLn "\n--- Detalhes dos Erros ---"
  mapM_ printLogEntry erros
```
- `when :: Monad m => Bool -> m () -> m ()`
- Executa ação apenas se condição for verdadeira
- Evita imprimir seção vazia se não houver erros

**Uso de `mapM_`:**
```haskell
mapM_ printLogEntry erros
```
- `mapM_ :: (a -> m b) -> [a] -> m ()`
- Aplica ação monádica (I/O) a cada elemento da lista
- `_` indica que descartamos o resultado (só queremos o efeito colateral de imprimir)

**Pattern matching em `Maybe`:**
```haskell
case Analise.itemMaisMovimentado logs of
  Nothing -> putStrLn "\nNenhum item movimentado ainda."
  Just (itemId, numOps) -> ...
```
- Trata caso onde não há logs (retorna `Nothing`)
- Extrai tupla `(itemId, numOps)` do `Just`

---

#### **repl: Loop Principal Interativo**

```haskell
repl :: Inventario -> [LogEntry] -> IO ()
repl inv logs = do
  line <- prompt "inventario> "
  let trimmed = dropWhile (== ' ') line
  if trimmed == "exit"
    then putStrLn "Encerrando..." >> return ()
    else do
      novoInv <- processCommand inv logs trimmed
      novosLogs <- loadLogs
      repl novoInv novosLogs  -- RECURSÃO!
```

**Explicação detalhada:**

**REPL = Read-Eval-Print Loop:**
1. **Read**: `prompt "inventario> "` lê entrada do usuário
2. **Eval**: `processCommand` executa o comando
3. **Print**: (feito dentro de `processCommand`)
4. **Loop**: `repl novoInv novosLogs` chama a si mesma recursivamente

**Recursão de cauda:**
- A última coisa que a função faz é chamar a si mesma
- Haskell otimiza isso (não estoura a pilha mesmo com milhares de comandos)

**Atualização de estado:**
```haskell
novoInv <- processCommand inv logs trimmed
novosLogs <- loadLogs
repl novoInv novosLogs
```
- `novoInv`: novo estado retornado por `processCommand`
- `novosLogs`: recarrega logs do disco (podem ter sido atualizados)
- Passa ambos para a próxima iteração

**Condição de parada:**
```haskell
if trimmed == "exit"
  then putStrLn "Encerrando..." >> return ()
```
- Quebra a recursão quando usuário digita "exit"
- `return ()`: retorna valor do tipo `IO ()` (necessário para type checking)

---

#### **main: Ponto de Entrada**

```haskell
main :: IO ()
main = do
  putStrLn "Inicializando sistema de inventario..."
  inv <- loadInventario
  logs <- loadLogs
  putStrLn $ "Inventario carregado: " ++ show (Map.size inv) ++ " itens."
  putStrLn $ "Logs carregados: " ++ show (length logs)
  putStrLn "Digite 'help' para ver comandos."
  repl inv logs
```

**Explicação detalhada:**

**Sequência de inicialização:**
1. Mensagem de boas-vindas
2. `loadInventario`: carrega estado do disco (ou inventário vazio)
3. `loadLogs`: carrega logs do disco (ou lista vazia)
4. Exibe estatísticas de carregamento
5. Inicia o REPL

**Operador `<-` vs `let`:**
```haskell
inv <- loadInventario     -- <- para ações IO
let tamanho = Map.size inv  -- let para valores puros
```

**Por que recarregar logs se já temos em memória?**
- Na primeira execução, mostra quantos logs existem de execuções anteriores
- Útil para auditoria e debugging

---

## Dados de Teste

Conforme requisito mínimo da especificação (seção 4.1), o sistema foi populado com 10 itens distintos para os testes:

| ID | Nome | Quantidade | Categoria |
|----|------|------------|-----------|
| 1 | Processador | 15 | Componentes |
| 2 | Mouse | 25 | Perifericos |
| 3 | Monitor 24 polegadas | 10 | Monitores |
| 4 | Webcam HD | 8 | Perifericos |
| 5 | Headset | 20 | Audio |
| 6 | SSD 500GB | 12 | Armazenamento |
| 7 | Memoria RAM 8GB | 30 | Componentes |
| 8 | Placa de Video | 5 | Componentes |
| 9 | Fonte 600W | 7 | Componentes |
| 10 | Gabinete ATX | 6 | Hardware |

**Item adicional para teste de erro (Cenário 2):**
- ID: 11, Nome: Teclado, Quantidade: 10, Categoria: Perifericos

---

## Cenários de Teste

Conforme especificação (seção 4.1), foram executados e documentados três cenários de teste manuais.

### Cenário 1: Persistência de Estado

**Objetivo:** Verificar se o sistema salva e carrega dados corretamente entre execuções.

**Ambiente inicial:** Sistema limpo, sem arquivos `Inventario.dat` ou `Auditoria.log`.

**Passos executados:**


1. Adicionados 3 itens via comando `add`:
   - ID: 1, Nome: Teclado, Qtd: 15, Cat: Perifericos
   - ID: 2, Nome: Mouse, Qtd: 25, Cat: Perifericos
   - ID: 3, Nome: Monitor 24 polegadas, Qtd: 10, Cat: Monitores

2. Verificado com comando `list` que os 3 itens estavam em memória

3. Programa fechado com comando `exit`

4. Verificação dos arquivos criados:
   -  `Inventario.dat` existe e contém Map com 3 itens
     
     <img width="1733" height="33" alt="image" src="https://github.com/user-attachments/assets/716ad9f7-7023-4d85-8765-88f7ce3297cf" />


   -  `Auditoria.log` existe e contém 3 entradas (uma por adição)

     
     <img width="1167" height="59" alt="image" src="https://github.com/user-attachments/assets/42a86e89-9080-4d2b-83a0-b3944e693e5d" />


5. Programa reiniciado

6. Imagem do output na prática:
<img width="1046" height="629" alt="Primeiro output" src="https://github.com/user-attachments/assets/0ae413db-802f-4334-b36c-f8c003c0d57e" />


**Ao reiniciar:**

<img width="748" height="239" alt="Segundo output" src="https://github.com/user-attachments/assets/e4f0c80d-3a97-4961-8669-cba489c3dcdc" />


**Conclusão:**
-  Estado foi persistido em `Inventario.dat`
-  Log de auditoria foi mantido em `Auditoria.log`
-  Todos os 3 itens foram carregados corretamente na próxima execução

---

### Cenário 2: Erro de Lógica (Estoque Insuficiente)

**Objetivo:** Verificar se o sistema trata corretamente tentativa de remover quantidade maior que o estoque disponível.

**Condição inicial:** Sistema populado com 10 itens (requisito de dados mínimos).

**Passos executados:**

1. Adicionado item de teste:
   - ID: 11
   - Nome: Teclado
   - Quantidade: 10
   - Categoria: Perifericos

2. Verificado nos arquivos que Teclado possui 10 unidades

3. Tentativa de remover 15 unidades:
   ```
   inventario> remove 11 15
   Falha: Erro: estoque insuficiente.
   ```

4. Sistema exibiu mensagem de erro clara e não travou

5. Programa fechado e arquivos verificados

6. Imagem do output na prática:
   <img width="742" height="310" alt="teste2" src="https://github.com/user-attachments/assets/132c3833-6fb4-4414-a9c3-8ca22f30011b" />


**Verificação do `Inventario.dat`:**
```
("11",Item {itemID = "11", nome = "Teclado", quantidade = 10, ...})
```
 Quantidade permanece 10

**Verificação do `Auditoria.log`:**
```
LogEntry {timestamp = ..., acao = Add, detalhes = "Item adicionado: Teclado (ID: 11)", status = Sucesso}
LogEntry {timestamp = ..., acao = Remove, detalhes = "Falha ao remover ID=11 qtd=15", status = Falha "Erro: estoque insuficiente."}
```
 Log registrou a operação com `status = Falha`

**Conclusão:**<img width="742" height="310" alt="teste2" src="https://github.com/user-attachments/assets/bc4b9ac8-c8f6-4c81-8563-ba1551d33478" />

-  Sistema detectou erro de lógica (estoque insuficiente)
-  Mensagem de erro exibida ao usuário
-  Log de auditoria registrou a tentativa com status de Falha
-  Sistema continua operacional após o erro

---

### Cenário 3: Geração de Relatório de Erros

**Objetivo:** Verificar se o comando `report` exibe corretamente os erros registrados, especialmente testando a função `logsDeErro`.

**Condição inicial:** Sistema com 11 itens e 12 entradas de log (incluindo 1 erro do Cenário 2).

**Passos executados:**

1. Programa reiniciado após Cenário 2
   ```
   Inventario carregado: 11 itens.
   Logs carregados: 12
   ```

2. Executado comando: `report`

**Resultado completo do relatório:**

<img width="912" height="911" alt="teste3" src="https://github.com/user-attachments/assets/a154bb5c-fc57-4d1e-87c5-e74c3986e9b6" />


**Análise das funções de relatório:**

 **Função `logsDeErro`:**
- Filtrou corretamente as entradas com status de Falha
- Exibiu apenas 1 erro (a tentativa de remover 15 unidades do Teclado)
- Formato da saída correto com timestamp, ação, detalhes e status

 **Função `historicoPorItem`:**
- Agrupou corretamente as operações por itemID
- 10 itens com 1 operação cada (Add)
- 1 item (Teclado) com 2 operações (Add + Remove falha)
- Total de 11 itens com movimentação

 **Função `itemMaisMovimentado`:**
- Identificou corretamente TECLADO como item mais movimentado
- Contagem correta: 2 operações

**Conclusão:**
-  Todas as funções de análise funcionam corretamente
-  O sistema gera relatórios analíticos precisos a partir dos logs de auditoria
-  A função `logsDeErro` filtra apenas entradas com `status = Falha`
-  O relatório fornece visão completa das operações do sistema

---


## Como executar:

1. Acesse o GDB online **[clicando aqui](https://onlinegdb.com/1yn1mQRTMc)**

2. Execute o programa clicando no botão "Run" no topo da tela.

3. No terminal, digite `help` para obter uma lista dos comandos, conforme a imagem abaixo:
<img width="1021" height="323" alt="image" src="https://github.com/user-attachments/assets/2338f88c-c74a-4a9a-ba15-159808fdc51a" />

4. Divirta-se com os testes!

6. Digite comandos conforme descrito na próxima seção

---

## Comandos Disponíveis

O sistema opera via terminal com os seguintes comandos:

### `help`
Exibe lista de comandos disponíveis.

**Uso:**
```
inventario> help
```

---

### `add`
Adiciona um novo item ao inventário.

**Uso:**
```
inventario> add
ID do item: 1
Nome: Teclado
Quantidade: 15
Categoria: Perifericos
Item adicionado com sucesso.
```

**Validações:**
- ID não pode estar duplicado
- Quantidade não pode ser negativa

---

### `remove <itemID> [quantidade]`
Remove quantidade de um item do inventário.

**Uso:**
```
inventario> remove 1 5
Item removido com sucesso.

inventario> remove 1
Item removido (15 unidades).
```

**Comportamento:**
- Com quantidade: remove a quantidade especificada
- Sem quantidade: remove todas as unidades do item

**Validações:**
- Item deve existir no inventário
- Quantidade não pode exceder estoque disponível
- Quantidade deve ser positiva

---

### `update <itemID> <quantidade>`
Atualiza a quantidade de um item para um novo valor absoluto.

**Uso:**
```
inventario> update 1 20
Quantidade atualizada com sucesso.
```

**Validações:**
- Item deve existir
- Nova quantidade não pode ser negativa

**Diferença de `remove`:**
- `remove 1 5`: subtrai 5 do estoque atual
- `update 1 5`: define estoque como exatamente 5

---

### `list`
Lista todos os itens do inventário atual em memória.

**Uso:**
```
inventario> list
Inventario atual:
1 | Teclado | qtd: 15 | cat: Perifericos
2 | Mouse | qtd: 25 | cat: Perifericos
```

---

### `report`
Gera relatório completo de auditoria com análises estatísticas.

**Uso:**
```
inventario> report
```

**Seções do relatório:**
1. Total de entradas de log
2. Total de operações com erro (usa `logsDeErro`)
3. Detalhes dos erros
4. Total de itens com movimentação
5. Histórico de operações por item (usa `historicoPorItem`)
6. Item mais movimentado (usa `itemMaisMovimentado`)

---

### `exit`
Encerra o programa.

**Uso:**
```
inventario> exit
Encerrando...
```
