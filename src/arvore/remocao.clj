(ns arvore.remocao (:require [arvore.arvore :refer :all]))

(defn pegaFilho1 [raiz] (if (nil? (get raiz :esquerda)) (get raiz :direita) (get raiz :esquerda)))

(defn pegaMaior [raiz] (if (nil? (get raiz :direita)) raiz (pegaMaior (get raiz :direita))))

(defn remover [raiz valor]
    (cond
        (nil? raiz) raiz ; se a arvore estiver vazia retorne ela mesma
        (= valor (pega raiz)) (do ; se o valor for igual ao valor do no atual 
            (cond 
                (folha? raiz) nil ; Se for folha, retorne nil para ficar como filho
                (= (qntFilhos raiz) 1) (pegaFilho1 raiz) ; Se tiver um filho, retorne o filho
                (= (qntFilhos raiz) 2) (
                    let [
                        maior (pegaMaior (get raiz :esquerda))
                        aux (pega maior)
                        subArv (remover raiz aux)]
                    (do
                    ;(print "maior =" maior "\naux = " aux "\nsubArv =" subArv "\nraiz =" raiz)
                    {:valor aux, :esquerda (get subArv :esquerda), :direita (get subArv :direita)})
                ))) ; Se tiver 2 filhos, siga para a função removePai2
        (< valor (pega raiz)) (assoc raiz :esquerda (remover (get raiz :esquerda) valor))
        (> valor (pega raiz)) (assoc raiz :direita (remover (get raiz :direita) valor))))

(defn gestaoRemove [raiz] (remover raiz (ler)))