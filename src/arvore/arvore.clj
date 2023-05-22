(ns arvore.arvore)

(defn pega [m] (get m :valor))

(defn ler [] (do (print "\nDigite um numero: ") (flush) (Integer/parseInt (read-line))))

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
        (do (inOrdem (get raiz :esquerda))
            (println (pega raiz)) 
            (inOrdem (get raiz :direita)))))

(defn posOrdem [raiz]
    (if (false? (nil? raiz)) 
        (do (posOrdem (get raiz :esquerda)) 
            (posOrdem (get raiz :direita))
            (println (pega raiz)))))

(defn conta [raiz] 
    (if (nil? raiz) 
        0 
        (+ 1 
            (conta (get raiz :esquerda))
            (conta (get raiz :direita)))))

(defn vazia? [raiz] (= (conta raiz) 0))

(defn gestaoShow [raiz op]
    (if (vazia? raiz) 
        (println "A arvore esta vazia")
        (cond
            (= op 1) (preOrdem raiz)
            (= op 2) (inOrdem raiz)
            (= op 3) (posOrdem raiz))))

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

(defn inserirMany [raiz n] (if (empty? n) raiz (inserirMany (insere (first n) raiz) (rest n))))

(defn gestaoRandom [raiz] (inserirMany raiz (map #(criaNo %) (take (ler) (repeatedly #(rand-int 1000))))))
