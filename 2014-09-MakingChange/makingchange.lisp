;; CLISP solution

(defun make-change (l amount)
    (cond
        ((not l) 0)
        ((< amount 0) 0)
        ((= amount 0) 1)
        (T (+ (make-change l (- amount (car l)))
              (make-change (cdr l) amount)))))

(defparameter *us-denominations* '(50 25 10 5 1))
(make-change *us-denominations* 100)

(defparameter *eu-denominations* '(50 20 10 5 2 1))
(make-change *eu-denominations* 100)
