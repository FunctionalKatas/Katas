(ns adder.core
  (require [clojure.string :as str]))

(defn parse-int
  "utility to convert a string to an integer"
  [s]
  (Integer. (re-find #"\d+" s)))

(defn add
  "accepts a string of numbers & a specified delimiter; adds the numbers"
  [s, delim]
  (if (identical? s "") 0                ; handle the case of a blank string
      (let [regex (re-pattern delim)     ; convert the delimiter to a regex
            str-vec (str/split s regex)  ; split the string into sub-strings
            nums (map parse-int str-vec) ; convert the strings to integers
            ans (reduce + 0 nums)]       ; add up the elements 
        ans)))

(defn super-add
  "accepts a string like  //:\n1:2:5
   treats the value between // and the first newline as the delimiter.
   parses the numbers following the first newline (using the delimiter);
   and adds up the numbers"
  [s]
  (let  [splitup (str/split s #"\n" )  ; split the input string on newlines
         h (first splitup)             ; the head
         t (apply str (rest splitup))  ; recombine the tail into a string
         delim (apply str (drop 2 h))  ; get the delimiter by dropping 2 chars
         ans (add t delim)]            ; call the adder to add the numbers
    ans))
