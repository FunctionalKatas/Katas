(ns fcats.core)

(use '[clojure.core.match :only (match)])


(defn count_change [amount coins]
  "Count the change"
  (match [amount coins]
       [0 _] 1
       [_ ([] :seq)] 0
       [(a :guard neg?) _] 0
       [_ _] (+ (count_change amount (rest coins)) (count_change (- amount (first coins)) coins))))


(count_change 100 [50 25 10  5 1])

