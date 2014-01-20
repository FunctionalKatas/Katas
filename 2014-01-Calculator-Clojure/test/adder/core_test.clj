(ns adder.core-test
  (:use midje.sweet) 
  (:require [adder.core :refer :all]))

(fact "parse-int converts a positive number-string into an integer"
    (parse-int "1") => 1
    (parse-int "12") => 12
    (parse-int "1221") => 1221)

(fact "add function accepts various delimiters; & adds the numbers"
    (add "1,2,3" ",") => 6
    (add "1;2;3" ";") => 6
    (add "1:2:3" ":") => 6)
    
(fact "add function accepts blank string and returns 0"
    (add "" ",") => 0 
    (add "" ";") => 0
    (add "" ":") => 0)

(fact "add function accepts multi-char delimiters"
    (add "1,;2" ",;") => 3 
    (add "15;;203;;15" ";;") => 233 
    (add ":;" ":;") => 0)

(fact "add function ignores \n inside the string"
    (add "1\n,;\n2" ",;") => 3 
    (add "15\n;;203;;\n15" ";;") => 233)

(fact "super-add will accept various delimiters at start of string"
    (super-add "//,\n1," ) => 1 
    (super-add "//;\n1;22") => 23
    (super-add "//:\n202:505:303") => 1010)

(fact "super-add ignores second and subsequent newlines"
    (super-add "//,\n1\n," ) => 1 
    (super-add "//;\n1\n;\n22") => 23
    (super-add "//:\n202\n:505:303\n") => 1010)

