; Exercise 1.20

;; applicative order

(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)

;; normal order
(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(if (= (remainder 206 40) 40)
    

; etc. it grows infinitely through substitution.

; no, wrong. if's are evaluated straight off!
