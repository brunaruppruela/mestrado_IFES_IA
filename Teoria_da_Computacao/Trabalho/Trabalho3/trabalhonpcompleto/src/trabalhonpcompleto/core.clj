(ns trabalhonpcompleto.core
  (:gen-class))

;(defn -main
; "I don't do a whole lot ... yet."
; [& args]
; (println "Hello, World!"))

(defn knapsack [weights values capacity]
  (let [n (count weights)]
    (loop [i 0
           w 0
           dp (vec #_{:clj-kondo/ignore [:deprecated-var]}
               (replicate (inc n) (vec (repeat (inc capacity) 0))))]
      (if (or (= i n) (= w capacity))
        (get-in dp [n capacity])
        (let [wi (nth weights i)
              vi (nth values i)]
          (if (>= w wi)
            (recur (inc i)
                   w
                   (assoc-in dp [i w] (max (get-in dp [i w])
                                           (+ (get-in dp [(dec i) (dec (- w wi))])
                                              vi))))
            (recur (inc i) w dp)))))))

(defn -main []
  (let [weights [2 5 4 1]
        values [3 4 5 6]
        capacity 5]
    (println "Max Value:" (knapsack weights values capacity))))

(-main)