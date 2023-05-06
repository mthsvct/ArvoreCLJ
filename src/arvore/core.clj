(ns arvore.core
	(:gen-class)
	(:require 
		[arvore.arvore :refer :all]
		[arvore.math :refer :all]))

(defn menu []
	(println)
	(println "1 - Inserir")
	(println "2 - Imprimir em Pre-Ordem")
	(println "3 - Imprimir em In-Ordem")
	(println "4 - Imprimir em Pos-Ordem")
	(println "5 - Buscar um valor" )
	(println "6 - Inserir valores aleatorios")
	(println "7 - Soma dos valores da arvore")
	(println "8 - Subtracao dos valores da arvore")
	(println "9 - Produto dos valores da arvore" )
	(println "10 - Quantidade de nos presentes na arvore")
	(println "11 - Média dos valores na arvore")
	(println "12 - Maior valor presente na arvore")
	(println "13 - Menor valor presente na arvore")
	(println "0 - Sair")
	(print "Digite uma opcao: ")
	(flush)
	(Integer/parseInt (read-line)))

(defn escolhe [raiz op] 
	(cond
		(= op 1)  (escolhe (gestaoLer raiz) (menu))
		(= op 2)  (do (preOrdem raiz) (escolhe raiz (menu)))
		(= op 3)  (do (inOrdem raiz) (escolhe raiz (menu)))
		(= op 4)  (do (posOrdem raiz) (escolhe raiz (menu)))
		(= op 5)  (do (gestaoBuscar raiz) (escolhe raiz (menu)))
		(= op 6)  (escolhe (gestaoRandom raiz) (menu))
		(= op 7)  (do (println (soma raiz)) (escolhe raiz (menu)))
		(= op 8)  (do (println (subt raiz)) (escolhe raiz (menu)))
		(= op 9)  (do (println (prod raiz)) (escolhe raiz (menu)))
		(= op 10) (do (println (conta raiz)) (escolhe raiz (menu)))
		(= op 11) (do (println (double (media raiz))) (escolhe raiz (menu)))
		(= op 12) (do (println (maior raiz)) (escolhe raiz (menu)))
		(= op 13) (do (println (menor raiz)) (escolhe raiz (menu)))
		(= op 0)  (do (println "Saindo...") raiz)
		:else     (do (println "Opcao invalida!") (escolhe raiz (menu)))))

; Sem usar loop
(defn -main [] (escolhe nil (menu)))

