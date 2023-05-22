(ns arvore.remocao (:require [arvore.arvore :refer :all]))

(defn nilInt? [filho] (if (nil? filho) 1 0))

(defn qntFilhos [raiz] (+ (nilInt? (get raiz :esquerda)) (nilInt? (get raiz :direita)) ))

(defn folha? [raiz] (= (qntFilhos raiz) 0))

(defn pegaFilho1 [raiz] (if (nil? (get raiz :esquerda)) (get raiz :direita) (get raiz :esquerda)))

(defn remover [raiz valor]
    (cond
        (nil? raiz) raiz ; se a arvore estiver vazia retorne ela mesma
        (= valor (pega raiz)) (do ; se o valor for igual ao valor do no atual 
            (cond 
                (folha? raiz) nil ; Se for folha, retorne nil para ficar como filho
                (= (qntFilhos raiz) 1) (pegaFilho1 raiz))) ; Se tiver um filho, retorne ele
        (< valor (pega raiz)) (assoc raiz :esquerda (remover (get raiz :esquerda) valor))
        (> valor (pega raiz)) (assoc raiz :direita (remover (get raiz :direita) valor))))

(defn gestaoRemove [raiz] (remover raiz (ler)))