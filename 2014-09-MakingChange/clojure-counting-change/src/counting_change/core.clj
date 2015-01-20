(ns counting-change.core)

(def COINS [ 50, 25, 10, 5, 1 ])

(defn count-the-ways 
 "count the number of ways to make change from amount using pocket of coins" 
  [amount, pocket] 
  (cond 
    (= amount 0) 1
    (< amount 0) 0
    (empty? pocket) 0
    :else (+ (count-the-ways amount, (pop pocket))
             (count-the-ways (- amount (peek pocket)), pocket))
   ))
