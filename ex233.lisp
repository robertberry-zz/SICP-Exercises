; Exercises for 2-3-3

(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((equal x (car set)) t)
        (t (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))

; Exercise 2.59

(defun union-set (set1 set2)
  (append (remove-if (lambda (x)
                       (element-of-set? x set2)) set1) set2))

; Exercise 2.60

;; element-of-set? stays the same. It may end up taking longer, however, as
;; for any given set, there is the potential for duplicate items to make 'n'
;; larger than the actual size of the set.

(defun adjoin-set-2 (x set)
  (cons x set))

;; Original adjoin-set was O(n) time. This adjoin set is O(1).

(defun union-set-2 (set1 set2)
  (append set1 set2))

;; Original union-set was O(n**2) time. This union set is O(n) time (as append
;; itself is O(n) time, n being the collective size of all but the last list).

;; intersection-set stays the same. You would still need to check that every
;; item in set1 is in set2. If anything it is slower, because you will be
;; re-checking duplicate items.

;; You might use this implementation if the program you were writing adjoins
;; sets very frequently and performs the other operations not so
;; frequently. But even then, adjoin-set was only O(n) time to begin with -
;; all of the other operations will now be slower, as 'n' will be larger than
;; the size of the set.

;; I would avoid this representation.

(defun element-of-ordered-set? (x set)
  (cond ((null set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-set? x (cdr set)))))

(defun intersection-of-ordered-set (set1 set2)
  (if (or (null set1) (null set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-of-ordered-set (cdr set1)
                                                     (cdr set2))))
              ((< x1 x2)
               (intersection-of-ordered-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-of-ordered-set set1 (cdr set2)))))))

; Exercise 2.61

(defun adjoin-set (x set)
  ())