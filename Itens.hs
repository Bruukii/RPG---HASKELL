-- Itens.hs
module Itens (usarItem) where

import Personagens (Personagem(..))

-- Função para usar um item
usarItem :: String -> [Personagem] -> (String, [Personagem])
usarItem item personagens = case item of
    "PocaoDeCura" -> -- Lógica para PocaoDeCura
        let atualizarVida personagem = if vida personagem < 100
                                        then personagem { vida = vida personagem + 20 }
                                        else personagem
        in ("Você usou PocaoDeCura.", map atualizarVida personagens)
    "CuraMaior" -> -- Lógica para PocaoDeCuraPotente
        let atualizarVida personagem = if vida personagem < 100
                                        then personagem { vida = vida personagem + 50 }
                                        else personagem
        in ("Você usou CuraMaior.", map atualizarVida personagens)
    _ -> ("Item desconhecido.", personagens)
