; Exercise 1.16

(defun square (n)
  (* n n))

;; recursive version
(defun fast-expt (b n)
  (cond ((= n 0) 1)
        ((evenp n) (square (fast-expt b (/ n 2))))
        (t (* b (fast-expt b (- n 1))))))

;; iterative version
(defun fast-expt-2 (b n)
  (labels ((fast-expt-iter (b n a)
             (cond ((= n 0) a)
                   ((evenp n) (fast-expt-iter (square b) (/ n 2) a))
                   (t (fast-expt-iter b (- n 1) (* a b))))))
    (fast-expt-iter b n 1)))

; Exercise 1.17

(defun dbl (n)
  (* n 2))

(defun halve (n)
  (/ n 2))

(defun multiply (a b)
  (cond ((= b 0) 0)
        ((evenp b) (multiply (dbl a) (halve b)))
        (t (+ a (multiply a (- b 1))))))

; Exercise 1.18

(defun multiply-2 (a b)
  (labels ((multiply-iter (a b acc)
             (cond ((= b 0) acc)
                   ((evenp b) (multiply-iter (dbl a) (halve b) acc))
                   (t (multiply-iter a (- b 1) (+ acc a))))))
    (multiply-iter a b 0)))

; Exercise 1.19

; T(p,q) = a <- bq + aq + ap
;          b <- bp + aq

; Show that the transformation applied twice can be described as T(p', q'), with
; p' and q' in terms of p and q

; T'(p,q) = a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;           a <- bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2
;           a <- 2bpq + 2aq^2 + bq^2 + 2apq + ap^2
;           a <- (2pq + q^2)b + (2pq + q^2)a + (p^2 + q^2)a

;           b <- (bp + aq)p + (bq + aq + ap)q
;           b <- bp^2 + aqp + bq^2 + aq^2 + apq
;           b <- bp^2 + 2aqp + bq^2 + aq^2
;           b <- (p^2 + q^2)b + (2qp + q^2)a

; T'(p,q) = T(p',q') where p' = (p^2 + q^2)
;                          q' = (2qp + q^2)

(defun fib (n)
  (fib-iter 1 0 0 1 n))

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
        ((evenp count) (fib-iter a
                                 b
                                 (+ (square p) (square q))
                                 (+ (* 2 q p) (square q))
                                 (/ count 2)))
        (t (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))