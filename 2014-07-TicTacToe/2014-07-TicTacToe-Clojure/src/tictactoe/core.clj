(ns tictactoe.core
  (:use clojure.set)
  )

(def x [ ])
(def o [ ])

(def board [ x o ])

; potential representations
; [ :x :o :x :o nil ]
;
; or sparse array:  [1 :x] [2 :o]
;
; indices of the board
; 1 2 3
; 4 5 6
; 7 8 9
;
; chose to segement the data-structure:
; one vector for x-player, one vector for o-player.
; integers represent the boxes already filled by the player:
; e.g. [ 1 9 ]
;
; win states
; 3 horizontal
; 3 vertical
; 2 diag.

; TBD
; (defn is-consistent
;  "detect if a given board state is consistent"
;  [ board ]
;  (  ))

(def win-states
  #{ #{ 1 2 3 }
     #{ 4 5 6 }
     #{ 7 8 9 }
     #{ 1 4 7 }
     #{ 2 5 8 }
     #{ 3 6 9 }
     #{ 1 5 9 }
     #{ 3 5 7 }
    })


(defn is-win?
  "detect if one player's board state is a winning position"
  [ x ]
  (some #(superset? x %) win-states))

