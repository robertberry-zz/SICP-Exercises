; 2.2.2

; Exercise 2.24

;; Goddammit D:

; a)

;  (1 (2 (3 4)))                       
                                       
; b)  _________     ________           
;     | . | ._|___\ | . |  /|          
;     |_|_|___|   / |_|_|/__|          
;       |             |                
;                    \|/               
;       1           ________      ________ 
;                   | . | ._|___\ | . |  /|
;                   |_|_|___|   / |_|_|/__|
;                     |             |      
;                                   |             
;                     2            \|/          
;                                 ________      ________ 
;                                 | . | ._|___\ | . |  /|
;                                 |_|_|___|   / |_|_|/__|
;                                   |             |     
;                                                       
;                                   3             4     
;                                                       

; c)
;                    (1 (2 (3 4)))
;                        /\
;                       /  \
;                      1   (2 (3 4)
;                          / \
;                         /   \
;                        2     (3 4)
;                               /  \
;                              3    4
;

; Exercise 2.25

; a) (1 3 (5 7) 9)
;
;    (car (cdr (car (cdr (cdr x)))))

; b) ((7))
;
;    (car (car x))

; c) (1 (2 (3 (4 (5 (6 7))))))
;
;    (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

; wtf that was mean :(

; Exercise 2.26

; a)

; (1 2 3 4 5 6)

; b)

; ((1 2 3) 4 5 6)

; c)

; ((1 2 3) (4 5 6))

; Exercise 2.27

(defun deep-reverse (x)
  (labels ((iter (x acc)
             (cond ((null x) acc)
                   ((atom x) x)
                   (t (iter (cdr x) (cons (iter (car x) '()) acc))))))
    (iter x '())))

;; Pretty proud of that one actually ... 

; Exercise 2.28

(defun fringe (x)
  (cond ((null x) '())
        ((atom x) (list x))
        (t (append (fringe (car x)) (fringe (cdr x))))))

; Exercise 2.29

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

; a)

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

; b)

(defun mobile? (structure)
  (listp structure))

(defun branch-weight (branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

(defun total-weight (mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; c)

(defun torque (branch)
  (* (branch-length branch)
     (branch-weight branch)))

(defun branch-balanced? (branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (balanced? structure)
        t)))

(defun balanced? (mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torque left) (torque right)))))

;; Some tests

(defparameter *balanced-mobile*
  (make-mobile (make-branch 5 (make-mobile (make-branch 2 2)
                                           (make-branch 2 2)))
               (make-branch 10 (make-mobile (make-branch 1 1)
                                            (make-branch 1 1)))))

(defparameter *unbalanced-mobile*
  (make-mobile (make-branch 5 (make-mobile (make-branch 2 2)
                                           (make-branch 2 2)))
               (make-branch 10 (make-mobile (make-branch 1 2)
                                            (make-branch 1 1)))))

;; CL-USER> (balanced? *balanced-mobile*)

;; T

;; CL-USER> (balanced? *unbalanced-mobile*)

;; NIL

;; Obviously, could do with a more thorough test suite ... 

; d)

;; Just the accessors need rewriting.

; Exercise 2.30

(defun square-tree (tree)
  (cond ((null tree) '())
        ((not (listp tree)) (* tree tree))
        (t (cons (square-tree (car tree))
                 (square-tree (cdr tree))))))

(defun square-tree-2 (tree)
  (mapcar (lambda (sub-tree)
            (if (listp sub-tree)
                (square-tree sub-tree)
                (* sub-tree sub-tree)))
          tree))

; Exercise 2.31

(defun tree-map (f tree)
  (mapcar (lambda (sub-tree)
            (if (listp sub-tree)
                (tree-map f sub-tree)
                (funcall f sub-tree)))
          tree))

(defun square-tree-3 (tree)
  (tree-map (lambda (x) (* x x)) tree))

; Exercise 2.32

(defun subsets (s)
  (if (null s)
      '(())
      (let ((rest (subsets (cdr s))))
        (append rest (mapcar (lambda (subset) (cons (car s) subset)) rest)))))

;; The subset of an empty set is just the empty set itself.

;; The subsets of a set is a union of:

;; The subsets that do not include the first element
;; and
;; The subsets that do include the first element.

;; We first calculate the subsets that do not include the first element
;; (through recursion - the end condition for this is the empty set, which
;; just returns the empty set).

;; To calculate the subsets that DO include the first element, we can simply
;; take these same subsets and append to each the first element.

;; This allows it to build up from the empty set the sets containing one
;; element, then those containing two, etc.

;; That's about as eloquently as I can put it unfortunately. ;P
