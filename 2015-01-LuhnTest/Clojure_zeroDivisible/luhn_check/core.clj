(ns luhn-check.core
  (:require [clojure.string :as string]))

(defn to-number [input]
  (#(Integer/parseInt (str %)) input))

(defn to-digits [input]
    (map to-number (str input)))

(defn double-if-even-element [index value]
  ;; we need to remember that indxing starts from 0, so element
  ;; with index 1 is even
  (if (odd? index)
    (* 2 value)
    value))

(defn double-every-second [input]
  (map-indexed double-if-even-element input))

(defn sum-digits [number]
  (reduce + (to-digits number)))

(defn divisible-by-ten? [number]
  (zero? (mod number 10)))

(defn luhn-check [input]
  (let [digits (to-digits input)
        doubled-numbers (double-every-second digits)
        summed-numbers (map sum-digits doubled-numbers)
        final-sum (reduce + summed-numbers)]
    (divisible-by-ten? final-sum)))
