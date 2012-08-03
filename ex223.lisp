; Exercises for 2-2-3

(defun enumerate-interval (i j)
  (if (> i j)
      '()
      (cons i (enumerate-interval (1+ i) j))))

(defun enumerate-tree (tree)
  (cond ((null tree) '())
        ((atom tree) '(tree))
        (t (append (enumerate-tree (car tree))
                   (enumerate-tree (cdr tree))))))

; Exercise 2.33

(defun accumulate (f initial seq)
  (if (null seq)
      initial
      (funcall f (car seq)
               (accumulate f initial (cdr seq)))))

(defun my-map (p sequence)
  (accumulate (lambda (x y) (cons (funcall p x) y)) '() sequence))

(defun my-append (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun my-length (seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

; Exercise 2.34

(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; Exercise 2.35

(defun const (x)
  "Returns a function that takes one parameter and always returns x."
  (lambda (y)
    x))

(defun count-leaves (tree)
  (accumulate '+ 0 (mapcar (const 1) (enumerate-tree tree))))

; Exercise 2.36

(defun accumulate-n (f init seqs)
  (if (null (car seqs))
      '()
      (cons (accumulate f init (mapcar #'car seqs))
            (accumulate-n f init (mapcar #'cdr seqs)))))

; Exercise 2.37

(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar (lambda (row)
            (dot-product row v)) m))

(defun transpose (mat)
  (accumulate-n #'cons '() mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar (lambda (row)
              (mapcar (lambda (col)
                        (dot-product col row)) cols)) m)))

;; fuck yeah!

; Exercise 2.38

(defun fold-left (op init seq)
  (labels ((iter (result rest)
             (if (null rest)
                 result
                 (iter (funcall op result (car rest))
                       (cdr rest)))))
    (iter init seq)))

; a)

;; (fold-right / 1 (list 1 2 3)) will be (3 / (2 / (1 / 1)))
;; so it should be 3/2

;; (fold-left / 1 (list 1 2 3)) will be 1 / (1 / (2 / 3))
;; so it should be 1/6

;; (fold-right list nil (list 1 2 3)) will be (list 1 (list 2 (list 3 nil)))
;; so it should be (1 (2 (3 nil)))

;; (fold-left list nil (list 1 2 3)) will be (list (list (list nil 1) 2) 3)
;; so it should be (((nil 1) 2) 3)

; b)

; Needs to be commutative.


; Exercise 2.39

(defun reverse-1 (seq)
  (accumulate (lambda (x y)
                (append y (list x))) nil seq))

(defun reverse-2 (seq)
  (fold-left (lambda (x y)
               (cons y x)) nil seq))

; Exercise 2.40

(defun flatmap (f seq)
  (accumulate #'append '() (mapcar f seq)))

(defun unique-pairs (n)
  (flatmap (lambda (i)
             (mapcar (lambda (j)
                       (list i j)) (enumerate-interval 1 i)))
           (enumerate-interval 1 n)))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime? (x)
  "TODO: grab this from one of the older exercises and insert it here ... "
  t)

(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
          (remove-if-not #'prime-sum?
                         (unique-pairs n))))

; Exercise 2.41

(defun ordered-triples (n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                        (mapcar (lambda (i)
                                  (list i j k)) (enumerate-interval 1 (1- j))))
                      (enumerate-interval 1 (1- k))))
           (enumerate-interval 1 n)))

(defun triples-summing-to (n s)
  (remove-if-not (lambda (triple)
                   (= (apply #'+ triple) s))
                 (ordered-triples n)))

; Exercise 2.42

(defun indexed-pairs (seq)
  (labels ((iter (seq i)
             (if (null seq)
                 '()
                 (cons (list i (car seq))
                       (iter (cdr seq) (1+ i))))))
    (iter seq 0)))


;; I did this with a much simpler representation for the board than they
;; expected, I think. (A list of numbers representing the row the Queen is in
;; for each column.)
(defun queens (board-size)
  (let ((empty-board '()))
    (labels ((adjoin-position (new-row k rest-of-queens)
               (cons new-row rest-of-queens))
             (safe? (k positions)
               (let ((new-position (car positions))
                     (rest (cdr positions)))
                 (every (lambda (pair)
                          (let ((distance (1+ (car pair)))
                                (position (cadr pair)))
                            (and (/= position new-position)
                                 (/= (+ position distance) new-position)
                                 (/= (- position distance) new-position))))
                        (indexed-pairs rest))))
             (queen-cols (k)
               (if (= k 0)
                   (list empty-board)
                   (remove-if-not
                    (lambda (positions)
                      (safe? k positions))
                    (flatmap
                     (lambda (rest-of-queens)
                       (mapcar (lambda (new-row)
                                 (adjoin-position new-row k rest-of-queens))
                               (enumerate-interval 1 board-size)))
                     (queen-cols (- k 1)))))))
      (queen-cols board-size))))

;; I'll do a print function instead for mah numeric representation. Gonna
;; rotate the board to make it easier to print out, 'cause I'm lazeh

(defun print-board (board)
  (let ((size (length board)))
    (mapcar (lambda (row)
              (format t "|~{~a|~}~%" 
                      (mapcar (lambda (i)
                                (if (= i row) "Q" " "))
                              (enumerate-interval 1 size))))
         board)))

; Exercise 2.43

;; Because it repeatedly recalculates (queen-cols (- k 1)) for every row in
;; the new column.

;; The work at each level of recursion is now performed n times.

;; Which means the work at level 1 is n times as large
;; The work at level 2 is n * n times as large
;; Etc.

;; Assuming T is a function based on the problem size, assume T(n^n). This is
;; a vague guess. I've not properly studied computational complexity. (That's
;; next after SICP.)

