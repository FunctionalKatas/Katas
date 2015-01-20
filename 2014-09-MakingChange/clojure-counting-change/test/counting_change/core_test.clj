(ns counting-change.core-test
  (:require [clojure.test :refer :all]
            [counting-change.core :refer :all]))

(deftest one-dollar-change
  (testing "can we make change of one dollar"
    (is (= 292 (count-the-ways 100 COINS)))))

(deftest ten-dollar-change
  (testing "can we make change of ten dollars"
    (is (= 801451 (count-the-ways 1000 COINS)))))
