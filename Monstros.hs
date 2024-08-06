module Monstros (
    Monstro(..), -- Exporta o tipo Monstro e seus construtores
    escolherMonstro
) where

import System.Random (randomRIO)

-- Definição do tipo Monstro
data Monstro = Monstro {
    tipoMonstro :: String,
    nomeMonstro :: String,
    vidaMonstro :: Int,
    ataqueMonstro :: Int
} deriving Show

-- Função para escolher um monstro aleatório com base na fase
escolherMonstro :: Int -> IO Monstro
escolherMonstro fase = do
    let monstros = case fase of
            1 -> [Monstro "Goblin" "Goblin" 30 10, Monstro "Esqueleto" "Esqueleto" 50 15]
            2 -> [Monstro "Troll" "Troll" 70 20, Monstro "Orc" "Orc" 60 25]
            3 -> [Monstro "Golem" "Golem" 90 30, Monstro "Fênix" "Fênix" 110 35]
            4 -> [Monstro "Kraken" "Kraken" 200 50, Monstro "Dragão" "Dragão" 150 40]
            _ -> [Monstro "Goblin" "Goblin" 30 10]
    idx <- randomRIO (0, length monstros - 1)
    return (monstros !! idx)
