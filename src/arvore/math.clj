(ns arvore.math (:require [arvore.arvore :refer [pega]]))

(defn soma [raiz] (if (nil? raiz) 0 (+ (pega raiz) (soma (get raiz :esquerda)) (soma (get raiz :direita)))))

(defn subt [raiz] (if (nil? raiz) 0 (- (pega raiz) (subt (get raiz :esquerda)) (subt (get raiz :direita)))))

(defn prod [raiz] (if (nil? raiz) 1 (* (pega raiz) (prod (get raiz :esquerda)) (prod (get raiz :direita)))))

(defn conta [raiz] (if (nil? raiz) 0 (+ 1 (conta (get raiz :esquerda)) (conta (get raiz :direita)))))

(defn media [raiz] (/ (soma raiz) (conta raiz)))

(defn maior [raiz] (if (false? (nil? raiz)) (if (nil? (get raiz :direita)) raiz (maior (get raiz :direita)))))

(defn menor [raiz] (if (false? (nil? raiz)) (if (nil? (get raiz :esquerda)) raiz (menor (get raiz :esquerda)))))