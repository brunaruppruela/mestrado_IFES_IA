(ns trabalhonp.core
  (:gen-class))

;(defn -main
; "I don't do a whole lot ... yet."
; [& args]
; (println "Hello, World!"));;

 ;função quee recebe três argumentos: values (valores dos itens), weights (pesos dos itens) e capacity (capacidade da mochila).
(defn knapsack [values weights capacity]
  ;definição das funções locais
  ;A função recursiva knapsack-helper recebe dois parâmetros, i (o índice do item atual) e remaining-capacity (a capacidade restante da mochila).
  (letfn [(knapsack-helper [i remaining-capacity] 
          ;1º IF: Verifica se chegamos ao final dos itens ou se a capacidade restante é zero. 
          ;Se qualquer uma das condições for verdadeira, o valor retornado é 0, 
          ;indicando que não há mais itens para adicionar ou espaço na mochila.                 
            (if (or (zero? i) (zero? remaining-capacity))
              0
              ;Calcula o valor máximo considerando o item atual (with-item) e 
              ;o valor máximo sem considerar o item atual (without-item).
              (let [without-item (knapsack-helper (dec i) remaining-capacity)
                    with-item (if (>= remaining-capacity (nth weights (dec i)))
                                (+ (nth values (dec i))
                                   (knapsack-helper (dec i) (- remaining-capacity (nth weights (dec i)))))
                                0)]
                ;Chamada recursiva: Retorna o máximo entre os dois resultados, representando a escolha de incluir ou excluir o item atual.
                (max without-item with-item))))]
    ;Chamada inicial: inicia a recursão com o índice inicial sendo o número total de itens e a capacidade total da mochila.
    (knapsack-helper (count values) capacity)))

(comment
;Define os valores dos itens.
 (def values [60 100 120])
 ;Define os pesos dos itens.
 (def weights [10 20 30])
;Define a capacidade da mochila.
 (def capacity 50)

;Chama a função knapsack com os valores, pesos e capacidade definidos, e mostra o resultado.
 (println "Valor máximo que pode ser obtido:" (knapsack values weights capacity)))

;Função melhorada para pegar as entradas do usuario
(defn -main []
  (println "Informe os valores dos itens, pesos e capacidade da mochila (Dê enter):")
  (let [values (mapv read-string (clojure.string/split (read-line) #"\s+"))
        weights (mapv read-string (clojure.string/split (read-line) #"\s+"))
        capacity (read-string (read-line))]
    (println "Valor máximo que pode ser obtido:" (knapsack values weights capacity))))

;; Chama a função -main quando o programa é executado
(-main)
