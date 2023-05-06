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

(defn busca [raiz valor passos]
    (cond
        (nil? raiz) {:no nil :passos passos}
        (= (pega raiz) valor) {:no raiz :passos passos}
        (< valor (pega raiz)) (busca (get raiz :esquerda) valor (inc passos))
        (> valor (pega raiz)) (busca (get raiz :direita) valor (inc passos))))

(defn apresentaBuscar [res]
    (cond
        (nil? (get res :no)) (println "O valor não foi encontrado" (str "Quantidade de passos " (get res :passos)))
        :else (println "O valor foi encontrado!" (get res :no) (str "Quantidade de passos " (get res :passos)))))

(defn gestaoBuscar [raiz] (apresentaBuscar (busca raiz (ler) 0)))