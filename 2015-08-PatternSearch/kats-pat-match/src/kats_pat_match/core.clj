(ns kats-pat-match.core
  (:require [clojure.string :as str]))

; get a vector of all the words in the dictionary
(def DICT (str/split (slurp "./resources/wordlist.txt") #"\n"))

(take 10 DICT)

(defn make-regex
  "Make a case-insensitive regex from a string"
  [s]
  (let [regex-str (str/replace (str/replace s #"\*" ".*") #"\?" ".?")]
    (re-pattern (str "(?i)" regex-str))))

(defn dict-match
  "workhorse function: from entire dictionary: return sequence of matching words"
  [pat, search-db]
  (let [regex (make-regex pat)]
    (filter (partial re-matches regex) search-db)))

(dict-match "cri?" DICT)

(count (dict-match "cri*" DICT))

(dict-match "cri*" DICT)




; (comment
;)

;(comment
  (make-regex "cri?")
  (make-regex "cri*")
; )

;(comment
 (pat-match (make-regex "hello*") "hello world")
 (pat-match (make-regex "helx*") "hello world")
;)

;  (:require [clojure.core.match :refer [match]])
