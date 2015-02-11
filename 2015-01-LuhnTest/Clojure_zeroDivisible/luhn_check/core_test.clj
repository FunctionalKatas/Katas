(ns luhn-check.core-test
  (:use midje.sweet
        luhn-check.core))

(facts "to-digits"
       (to-digits 1) => '(1)
       (to-digits 12) => '(1 2)
       (to-digits 123) => '(1 2 3))

(facts "double-every-second"
       (double-every-second '(1 2 3 4)) => '(1 4 3 8)
       (double-every-second '(4 3 2 1)) => '(4 6 2 2))

(facts "sum-digits"
       (sum-digits 1) => 1
       (sum-digits 5) => 5
       (sum-digits 10) => 1
       (sum-digits 12) => 3
       (sum-digits 18) => 9)

(facts "luhn-check"
       (luhn-check 61789372994) => true
       (luhn-check 79927398713) => true
       (luhn-check 61789372993) => false)
