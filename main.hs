module Main where

import System.IO (hFlush, stdout)
import qualified Monstros as M
import Personagens
import Itens
import Combate

-- Função para exibir a tela inicial
telaInicial :: IO ()
telaInicial = do
    putStrLn "Bem-vindo ao Simulador de Aventura RPG!"
    putStrLn "1. Jogar"
    putStrLn "2. Sair"
    putStrLn "Escolha uma opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> iniciarJogo
        "2" -> putStrLn "Saindo do jogo..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            telaInicial

-- Função para iniciar o jogo
iniciarJogo :: IO ()
iniciarJogo = do
    putStrLn "Crie seu primeiro personagem:"
    pers1 <- criarPersonagem
    putStrLn "Personagem criado!"
    print pers1

    putStrLn "\nCrie seu segundo personagem:"
    pers2 <- criarPersonagem
    putStrLn "Personagem criado!"
    print pers2

    putStrLn "\nIniciando a primeira fase..."
    jogoDeAventura [pers1, pers2] 1

-- Função para o loop do jogo de aventura
jogoDeAventura :: [Personagem] -> Int -> IO ()
jogoDeAventura personagens fase = do
    monstro <- M.escolherMonstro fase
    putStrLn $ "\nFase " ++ show fase ++ ": Um " ++ M.nomeMonstro monstro ++ " apareceu!"

    -- Inicia o combate
    combateLoop personagens monstro

  where
    combateLoop :: [Personagem] -> M.Monstro -> IO ()
    combateLoop personagens monstro = do
        -- Verifica se o monstro ou algum personagem foi derrotado antes de começar
        let (personagem1:personagem2:_) = personagens
        if M.vidaMonstro monstro <= 0 || all (\p -> vida p <= 0) personagens
          then do
            putStrLn "O combate terminou"
            if all (\p -> vida p <= 0) personagens
              then do
                putStrLn "Todos os jogadores morreram. Fim de jogo."
                finalizarJogo
              else return ()
          else do
            -- Ação do jogador 1
            putStrLn $ nome personagem1 ++ ", escolha a ação (1. Atacar, 2. Inventário, 3. Fugir):"
            hFlush stdout
            acao1 <- getLine
            resultado1 <- case acao1 of
              "1" -> do
                let novoMonstro = combateComPersonagens personagens monstro
                if M.vidaMonstro novoMonstro <= 0
                  then do
                    putStrLn "O monstro foi derrotado! Vocês venceram a fase!"
                    let proximaFase = fase + 1
                    if proximaFase <= 4
                      then do
                        putStrLn $ "\nPreparando a fase " ++ show proximaFase ++ "..."
                        jogoDeAventura personagens proximaFase
                      else do
                        putStrLn "Parabéns! Vocês completaram todas as fases!"
                        finalizarJogo
                    return Nothing
                  else do
                    let novosPersonagens = map (sofrerAtaqueDoMonstro novoMonstro) personagens
                    putStrLn "Estado dos personagens após o ataque do monstro:"
                    mapM_ (putStrLn . mostrarVida) novosPersonagens
                    putStrLn $ "Vida do " ++ M.nomeMonstro novoMonstro ++ ": " ++ show (M.vidaMonstro novoMonstro)
                    return (Just (novosPersonagens, novoMonstro))

              "2" -> do
                putStrLn "Escolha um item do inventário para usar (exemplo: PocaoDeCura, CuraMaior):"
                hFlush stdout
                itemEscolhido <- getLine
                let (item, novosPersonagens) = usarItem itemEscolhido personagens
                putStrLn $ "Você usou " ++ item ++ "."
                putStrLn "Estado dos personagens após usar o item:"
                mapM_ (putStrLn . mostrarVida) novosPersonagens
                putStrLn $ "Vida do " ++ M.nomeMonstro monstro ++ ": " ++ show (M.vidaMonstro monstro)
                return (Just (novosPersonagens, monstro))

              "3" -> do
                putStrLn "Você fugiu da batalha. Fim de jogo."
                finalizarJogo
                return Nothing

              _ -> do
                putStrLn "Ação inválida. Tente novamente."
                return (Just (personagens, monstro))

            case resultado1 of
              Nothing -> return ()
              Just (novosPersonagens1, novoMonstro) -> do
                -- Verifica novamente se o jogo terminou após a ação do jogador 1
                if M.vidaMonstro novoMonstro <= 0 || all (\p -> vida p <= 0) novosPersonagens1
                  then do
                    if M.vidaMonstro novoMonstro <= 0
                      then return ()
                      else do
                        putStrLn "Todos os jogadores morreram. Fim de jogo."
                        finalizarJogo
                  else do
                    -- Ação do jogador 2
                    putStrLn $ nome personagem2 ++ ", escolha a ação (1. Atacar, 2. Inventário, 3. Fugir):"
                    hFlush stdout
                    acao2 <- getLine
                    resultado2 <- case acao2 of
                      "1" -> do
                        let novoMonstro2 = combateComPersonagens novosPersonagens1 novoMonstro
                        if M.vidaMonstro novoMonstro2 <= 0
                          then do
                            putStrLn "O monstro foi derrotado! Vocês venceram a fase!"
                            let proximaFase = fase + 1
                            if proximaFase <= 4
                              then do
                                putStrLn $ "\nPreparando a fase " ++ show proximaFase ++ "..."
                                jogoDeAventura novosPersonagens1 proximaFase
                              else do
                                putStrLn "Parabéns! Vocês completaram todas as fases!"
                                finalizarJogo
                            return Nothing
                          else do
                            let novosPersonagens2 = map (sofrerAtaqueDoMonstro novoMonstro2) novosPersonagens1
                            putStrLn "Estado dos personagens após o ataque do monstro:"
                            mapM_ (putStrLn . mostrarVida) novosPersonagens2
                            putStrLn $ "Vida do " ++ M.nomeMonstro novoMonstro2 ++ ": " ++ show (M.vidaMonstro novoMonstro2)
                            return (Just (novosPersonagens2, novoMonstro2))

                      "2" -> do
                        putStrLn "Escolha um item do inventário para usar (exemplo: PocaoDeCura, CuraMaior):"
                        hFlush stdout
                        itemEscolhido <- getLine
                        let (item, novosPersonagens2) = usarItem itemEscolhido novosPersonagens1
                        putStrLn $ "Você usou " ++ item ++ "."
                        putStrLn "Estado dos personagens após usar o item:"
                        mapM_ (putStrLn . mostrarVida) novosPersonagens2
                        putStrLn $ "Vida do " ++ M.nomeMonstro novoMonstro ++ ": " ++ show (M.vidaMonstro novoMonstro)
                        return (Just (novosPersonagens2, novoMonstro))

                      "3" -> do
                        putStrLn "O jogador 2 fugiu da batalha. Fim de jogo."
                        finalizarJogo
                        return Nothing

                      _ -> do
                        putStrLn "Ação inválida. Tente novamente."
                        return (Just (novosPersonagens1, novoMonstro))

                    case resultado2 of
                      Nothing -> return ()
                      Just (novosPersonagens2, novoMonstro2) ->
                        -- Continua o loop do combate com o novo estado
                        combateLoop novosPersonagens2 novoMonstro2

-- Função para finalizar o jogo
finalizarJogo :: IO ()
finalizarJogo = do
    putStrLn "\nObrigado por jogar o Simulador de Aventura RPG!"
    putStrLn "Esperamos que tenha se divertido e que tenha sido uma grande aventura."
    putStrLn "Até a próxima!"
    putStrLn "Pressione Enter para sair."
    hFlush stdout
    _ <- getLine  -- Aguarda o usuário pressionar Enter
    putStrLn "Jogo finalizado."
    return ()

-- Função para exibir apenas os pontos de vida dos personagens
mostrarVida :: Personagem -> String
mostrarVida p = "Vida do " ++ nome p ++ ": " ++ show (vida p)

-- Função principal do programa
main :: IO ()
main = telaInicial
