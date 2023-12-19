(ns trabalhonp.heuristica
  (:gen-class))

(defn knapsack-greedy [values weights capacity]
  (let [n (count values)
        ratios (map / values weights)
        sorted-items (map vector values weights ratios)
        sorted-items (sort-by #(> (last %)) sorted-items)] ; Ordena por valor/peso decrescente
    (loop [remaining-capacity capacity
           total-value 0
           items []]
      (if (and (not (empty? sorted-items)) (> remaining-capacity 0))
        (let [item (first sorted-items)
              value (first item)
              weight (second item)
              ratio (last item)]
          (if (<= remaining-capacity weight)
            (recur 0 total-value (conj items [remaining-capacity value weight ratio]))
            (recur (- remaining-capacity weight) (+ total-value value) (conj items item))))
        [total-value items]))))

(defn -main []
  (println "Informe os valores dos itens (separados por espaços):")
  (let [values (mapv read-string (clojure.string/split (read-line) #"\s+"))
        weights (mapv read-string (clojure.string/split (read-line) #"\s+"))
        capacity (read-string (read-line))]
    (let [[total-value selected-items] (knapsack-greedy values weights capacity)]
      (println "Valor máximo que pode ser obtido:", total-value)
      (println "Itens selecionados:", selected-items))))

;; Chama a função -main quando o programa é executado
(-main)
