(ns arvore.core
	(:gen-class)
	(:require 
		[arvore.arvore :refer :all] 
		[arvore.math :refer :all] 
		[arvore.remocao :refer :all]))

(defn menu []
	(println)
	(println "1 - Inserir")
	(println "2 - Imprimir em Pre-Ordem")
	(println "3 - Imprimir em In-Ordem")
	(println "4 - Imprimir em Pos-Ordem")
	(println "5 - Imprimir em In-Ordem Reverse")
	(println "6 - Buscar um valor" )
	(println "7 - Inserir valores aleatorios")
	(println "8 - Soma dos valores da arvore")
	(println "9 - Subtracao dos valores da arvore")
	(println "10 - Produto dos valores da arvore" )
	(println "11 - Quantidade de nos presentes na arvore")
	(println "12 - Média dos valores na arvore")
	(println "13 - Maior valor presente na arvore")
	(println "14 - Menor valor presente na arvore")
	(println "15 - Remover um valor da arvore")
	(println "16 - Altura da arvore")
	(println "17 - Profundidade de um nó")
	(println "0 - Sair")
	(print "Digite uma opcao: ")
	(flush)
	(Integer/parseInt (read-line)))

(defn escolhe [raiz op] 
	(cond
		(= op 1)  (escolhe (gestaoLer raiz) (menu))
		(= op 2)  (do (gestaoShow raiz 1) (escolhe raiz (menu)))
		(= op 3)  (do (gestaoShow raiz 2) (escolhe raiz (menu)))
		(= op 4)  (do (gestaoShow raiz 3) (escolhe raiz (menu)))
		(= op 5)  (do (gestaoShow raiz 4) (escolhe raiz (menu)))
		(= op 6)  (do (gestaoBuscar raiz) (escolhe raiz (menu)))
		(= op 7)  (escolhe (gestaoRandom raiz) (menu))
		(= op 8)  (do (println (soma raiz)) (escolhe raiz (menu)))
		(= op 9)  (do (println (subt raiz)) (escolhe raiz (menu)))
		(= op 10)  (do (println (prod raiz)) (escolhe raiz (menu)))
		(= op 11) (do (println (conta raiz)) (escolhe raiz (menu)))
		(= op 12) (do (println (double (media raiz))) (escolhe raiz (menu)))
		(= op 13) (do (println (maior raiz)) (escolhe raiz (menu)))
		(= op 14) (do (println (menor raiz)) (escolhe raiz (menu)))
		(= op 15) (escolhe (gestaoRemove raiz) (menu))
		(= op 16) (do (println (altura raiz)) (escolhe raiz (menu)))
		(= op 17) (do (gestaoProfundidade raiz) (escolhe raiz (menu)))
		(= op 0)  (do (println "Saindo...") raiz)
		:else     (do (println "Opcao invalida!") (escolhe raiz (menu)))))

; Sem usar loop
(defn -main [] (do (println "ARVORE") (escolhe nil (menu))))

