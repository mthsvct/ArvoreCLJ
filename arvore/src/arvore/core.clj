(ns arvore.core
	(:gen-class)
	(:require [arvore.arvore :refer :all]))


(defn menu []
	(println "1 - Inserir")
	(println "2 - Imprimir em Pre-Ordem")
	(println "3 - Imprimir em In-Ordem")
	(println "4 - Imprimir em Pos-Ordem")
	(println "0 - Sair")
	(print "Digite uma opcao: ")
	(flush)
	(Integer/parseInt (read-line)))

(defn escolhe [raiz op] 
	(do
		(cond
			(= op 1) (escolhe (gestaoLer raiz) (menu))
			(= op 2) (do (preOrdem raiz) (escolhe raiz (menu)))
			(= op 3) (do (inOrdem raiz) (escolhe raiz (menu)))
			(= op 4) (do (posOrdem raiz) (escolhe raiz (menu)))
			(= op 0) (do (println "Saindo...") raiz)
			:else (do (println "Opcao invalida!") (escolhe raiz (menu))))))

; Sem usar loop
(defn -main [] 
	(println (escolhe nil (menu)))
)

