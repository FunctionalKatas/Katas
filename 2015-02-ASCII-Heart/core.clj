(ns love.core
  (:gen-class))

;;Challenge: to draw an ASCII heart using the letters of the word 'love'
;;This is a non-geometric approach. Based on these steps:
;;  1. Define a function (gimme) that takes a number and a string and returns a
;;     string made of that number of instances of the input-string, strung together.
;; 
;;  2. Making use of that function, create a string of six lines consisting of
;;     spaces, tabs and the word "love", arranged in the shape of the top portion
;;     of a heart shape, including line-terminator characters. 
;;
;;  3. Define a recursive function to create the 'V' shape at the bottom
;;     of a love heart, again making use of the 'gimme' function.
;;
;;     Not the most beautiful way to do it but I'm happy with my
;;     newbie code.

;;function: returns a string made of num iterations of string.
(defn gimme [num string] (reduce str (take num (repeat string))))

;;returns the two hill-shaped thingies at the top of a love heart shape
(defn make-two-hills []
  
  ;;did these manually because each line is a little different.
    (let [line-1 (str (gimme 3 \tab) "love" (gimme 24 \space) "love" \newline)
        line-2 (str (gimme 2 \tab) (gimme 4 "love") (gimme 2 \tab) (gimme 4 "love") \newline)
        line-3 (str \tab (gimme 7 "love") (gimme 8 \space) (gimme 7 "love") \newline)
        line-4 (str (gimme 4 \space) (gimme 18 "love") \newline)
        line-5 (str (gimme 2 \space) (gimme 19 "love") \newline)
        line-6 (str  (gimme 20 "love") \newline)]
    (str line-1 line-2 line-3 line-4 line-5 line-6)))

;;returns a v-shape at the bottom of a love heart shape.
(defn make-v []
  (loop [num-spaces   0
         num-loves    20
         total-string ""]
    (if (<= num-spaces 40)
      (recur (+ 2 num-spaces)
             (dec num-loves)
             (str total-string (gimme num-spaces " ")  (gimme num-loves  "love") \newline))
      (str total-string))))

;;join the top and bottom together and prints.  
(defn make-love-not-war []  
  (println (str (make-two-hills) (make-v))))

(defn -main
  [& args]
  ;;call the function and feel the love...
  (make-love-not-war))
