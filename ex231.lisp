; Answers for 2-3-1

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t (memq item (cdr x)))))

; Exercise 2.53

; (a b c)

; ((george))

; ((y1 y2))

; (y1 y2)

; false

; false

; (red shoes blue socks)


; Exercise 2.54

(defun equal? (a b)
  (cond ((null a) (null b))
        ((null b) (null a))
        ((atom a) (eq a b))
        (t (and (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))))))


; Exercise 2.55

;; It's double-quoted. ' is actually syntactic sugar for (quote <expr). So
;; what you actually have is,

;; (quote (quote abracadabra))

;; Which produces the list (quote abracadabra)

