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
  (let [weights [1 1 1 1]
        values [1 1 1 1]
        capacity 1]
    (println "Max Value dd:" (knapsack weights values capacity))))

(-main)
