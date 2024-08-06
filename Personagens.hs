module Personagens where

-- Definição de tipos de personagens
data TipoPersonagem = Guerreiro | Mago | Arqueiro deriving (Show, Read)

-- Definição de Habilidade
data Habilidade = Curar Int | BolaDeFogo Int | AtaquePesado Int | FlechaEnvenenada Int | FlechasCerteiras Int | Bloquear deriving Show

-- Definição de Item
data Item = PocaoDeCura Int | CuraMaior Int deriving Show

-- Definição de Inventário
data Inventario = Inventario {
    itens :: [Item]
} deriving Show

-- Definição de Personagem
data Personagem = Personagem {
    tipo       :: TipoPersonagem,
    nome       :: String,
    vida       :: Int,
    habilidades :: [Habilidade],
    inventario :: Inventario
} deriving Show

-- Função para criar personagens com inventário
criarPersonagem :: IO Personagem
criarPersonagem = do
    putStrLn "Digite o nome do seu personagem:"
    nomePers <- getLine
    putStrLn "Escolha o tipo do seu personagem:"
    putStrLn "1. Guerreiro"
    putStrLn "2. Mago"
    putStrLn "3. Arqueiro"
    tipoPersStr <- getLine
    let tipoPers = case tipoPersStr of
                      "1" -> Guerreiro
                      "2" -> Mago
                      "3" -> Arqueiro
                      _   -> error "Tipo de personagem inválido. Escolha 1, 2 ou 3."
    let habilidades = case tipoPers of
                        Guerreiro -> [AtaquePesado 20, Bloquear]
                        Mago -> [BolaDeFogo 30, Curar 10]
                        Arqueiro -> [FlechaEnvenenada 15, FlechasCerteiras 25]
    let inventario = Inventario [PocaoDeCura 20, CuraMaior 50] -- Exemplo de itens
    return Personagem { tipo = tipoPers, nome = nomePers, vida = 100, habilidades = habilidades, inventario = inventario}
