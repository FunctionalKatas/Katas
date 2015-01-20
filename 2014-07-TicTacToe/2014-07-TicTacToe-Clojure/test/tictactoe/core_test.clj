(ns tictactoe.core-test
  (:require [clojure.test :refer :all]
            [tictactoe.core :refer :all]))

(deftest win-state1
  (testing ""
    (is (is-win? #{1 2 3} ))))

(deftest fail-state1
  (testing ""
    (is not (is-win? #{1 2} ))))

(deftest win-state2
  (testing ""
    (is (is-win? #{1 5 9} ))))

(deftest fail-state2
  (testing ""
    (is not (is-win? #{1 4 9} ))))
