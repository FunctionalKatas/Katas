(ns functional-kats.luhn)

(defn make-integer [character]
  (Integer. (str character)))

(defn split-to-digits
  [num]
  (map make-integer (seq (str num))))

(defn luhn-reverse
  [num]
  (reverse (split-to-digits num)))

(defn merge-digits
  [num]
  (reduce + (split-to-digits num)))

(defn sum-digits-in-list
  [num-list]
  (map merge-digits num-list))

(defn even-indexed-digits
  [num]
  (take-nth 2 (rest (luhn-reverse num))))

(defn odd-indexed-digits
  [num]
  (take-nth 2 (luhn-reverse num)))

(defn luhn-double-even
  [num]
  (map #(* % 2) (even-indexed-digits num)))

(defn odd-indexed-bit
  [num]
  (reduce + (odd-indexed-digits num)))

(defn even-indexed-bit
  [num]
  (reduce +
          (sum-digits-in-list
           (luhn-double-even num))))

(defn ends-in-zero?
  [num]
  (zero? (mod num 10)))

(defn luhn? [num]
  (let [s1 (odd-indexed-bit num)
        s2 (even-indexed-bit num)
        sum (+ s1 s2)]
    (ends-in-zero? sum)))

(defn test-luhn []
  (map luhn? '(49927398716
               49927398717
               1234567812345678
               1234567812345670)))
