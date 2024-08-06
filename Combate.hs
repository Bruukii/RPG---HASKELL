module Combate (
    combate,
    combateComPersonagens,
    aplicarHabilidadeNoMonstro,
    sofrerAtaqueDoMonstro
) where

import Personagens
import Monstros

-- Função para realizar o combate entre todos os personagens e o monstro
combateComPersonagens :: [Personagem] -> Monstro -> Monstro
combateComPersonagens personagens monstro = foldl (\m pers -> combate pers m) monstro personagens

-- Função para realizar o combate entre um personagem e o monstro
combate :: Personagem -> Monstro -> Monstro
combate pers monstro = foldl (flip aplicarHabilidadeNoMonstro) monstro (habilidades pers)

-- Função para aplicar a habilidade do personagem no monstro
aplicarHabilidadeNoMonstro :: Habilidade -> Monstro -> Monstro
aplicarHabilidadeNoMonstro (Curar _) monstro = monstro -- Cura não afeta o monstro
aplicarHabilidadeNoMonstro (BolaDeFogo dano) monstro = monstro { vidaMonstro = vidaMonstro monstro - dano }
aplicarHabilidadeNoMonstro (AtaquePesado dano) monstro = monstro { vidaMonstro = vidaMonstro monstro - dano }
aplicarHabilidadeNoMonstro (FlechaEnvenenada dano) monstro = monstro { vidaMonstro = vidaMonstro monstro - dano }
aplicarHabilidadeNoMonstro (FlechasCerteiras dano) monstro = monstro { vidaMonstro = vidaMonstro monstro - dano }
aplicarHabilidadeNoMonstro Bloquear monstro = monstro

-- Função para aplicar o ataque do monstro nos personagens
sofrerAtaqueDoMonstro :: Monstro -> Personagem -> Personagem
sofrerAtaqueDoMonstro monstro pers = pers { vida = vida pers - ataqueMonstro monstro }
