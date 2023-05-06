(ns arvore.arvore)

(defn pega [m] (get m :valor))

(defn ler [] (do (print "Digite um numero: ") (flush) (Integer/parseInt (read-line))))

(defn criaNo [valor] {:valor valor :esquerda nil :direita nil})

(defn insere [novo raiz]
    (cond (nil? raiz) novo
        (< (pega novo) (pega raiz)) (assoc raiz :esquerda (insere novo (get raiz :esquerda)))
        (> (pega novo) (pega raiz)) (assoc raiz :direita (insere novo (get raiz :direita)))
        :else raiz))

(defn gestaoLer [raiz] (insere (criaNo (ler)) raiz))

(defn preOrdem [raiz] 
    (if (false? (nil? raiz)) 
        (do 
            (println (pega raiz)) 
            (preOrdem (get raiz :esquerda)) 
            (preOrdem (get raiz :direita)))))

(defn inOrdem [raiz]
    (if (false? (nil? raiz)) 
        (do 
            (inOrdem (get raiz :esquerda))
            (println (pega raiz)) 
            (inOrdem (get raiz :direita)))))

(defn posOrdem [raiz]
    (if (false? (nil? raiz)) 
        (do 
            (posOrdem (get raiz :esquerda)) 
            (posOrdem (get raiz :direita))
            (println (pega raiz)))))
