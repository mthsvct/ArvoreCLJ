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

(defn nilInt? [filho] (if (nil? filho) 0 1))

(defn qntFilhos [raiz] (+ (nilInt? (get raiz :esquerda)) (nilInt? (get raiz :direita))))

(defn folha? [raiz] (= (qntFilhos raiz) 0))

(defn show [raiz] "Apresenta infos do nó" (println (pega raiz) "-" (qntFilhos raiz) "filhos") )

(defn preOrdem [raiz] 
    "Imprime a arvore em pre-ordem"
    (if (false? (nil? raiz)) 
        (do (show raiz)
            (preOrdem (get raiz :esquerda)) 
            (preOrdem (get raiz :direita)))))

(defn inOrdem [raiz]
    "Imprime a arvore em ordem"
    (if (false? (nil? raiz)) 
        (do (inOrdem (get raiz :esquerda))
            (show raiz)
            (inOrdem (get raiz :direita)))))

(defn posOrdem [raiz]
    "Imprime a arvore em pos-ordem"
    (if (false? (nil? raiz)) 
        (do (posOrdem (get raiz :esquerda)) 
            (posOrdem (get raiz :direita))
            (show raiz))))
    
(defn inOrdemReverse [raiz]
    "Imprime a arvore em ordem reversa"
    (if (false? (nil? raiz)) 
        (do (inOrdemReverse (get raiz :direita))
            (println (pega raiz)) 
            (inOrdemReverse (get raiz :esquerda)))))

(defn conta [raiz] 
    "Conta a quantidade de nos da arvore"
    (if (nil? raiz) 
        0 
        (+  1 
            (conta (get raiz :esquerda))
            (conta (get raiz :direita)))))

(defn vazia? [raiz] (= (conta raiz) 0))

(defn gestaoShow [raiz op]
    (if (vazia? raiz) 
        (println "A arvore esta vazia")
        (cond
            (= op 1) (preOrdem raiz)
            (= op 2) (inOrdem raiz)
            (= op 3) (posOrdem raiz)
            (= op 4) (inOrdemReverse raiz))))

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

(defn altura [raiz] (if (nil? raiz) 0 (inc (max (altura (get raiz :esquerda)) (altura (get raiz :direita))))))

(defn profundidade [raiz valor cont]
    "Função que retorna a profundidade de um nó"
    (cond
        (nil? raiz) -1
        (= valor (pega raiz)) cont
        (> valor (pega raiz)) (profundidade (get raiz :direita) valor (inc cont))
        :else (profundidade (get raiz :esquerda) valor (inc cont))))

(defn gestaoProfundidade [raiz] 
    (let 
        [p (profundidade raiz (ler) 0)]
        (if (= p -1) 
            (println "Nao foi possivel calcular profundidade deste no pois o valor lido nao encontrado.")
            (println "Profunidade = " p))))