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

(defun adjoin-ordered-set (x set)
  (if (null set)
      (list x)
      (let ((fst (car set))
            (rst (cdr set)))
        (cond ((= x fst) set)
              ((> x fst) (cons fst (adjoin-set x rst)))
              ((< x fst) (list* x fst rst))))))

; Exercise 2.62

(defun union-ordered-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((x1 (car set1))
                 (x2 (car set2))
                 (xs1 (cdr set1))
                 (xs2 (cdr set2)))
             (cond ((= x1 x2) (cons x1 (union-ordered-set xs1
                                                          xs2)))
                   ((< x1 x2) (cons x1 (union-ordered-set xs1 set2)))
                   ((> x1 x2) (cons x2 (union-ordered-set set1 xs2))))))))

; --

(defun entry (tree)
  (car tree))

(defun left-branch (tree)
  (cadr tree))

(defun right-branch (tree)
  (caddr tree))

(defun make-tree (entry left right)
  (list entry left right))

(defun element-of-tree-set? (x set)
  (cond ((null set) nil)
        ((= x (entry set)) t)
        ((< x (entry set))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))

(defun adjoin-tree-set (x set)
  (cond ((null set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))


; Exercise 2.63

(defun tree->list-1 (tree)
  (if (null tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree result-list)
             (if (null tree)
                 result-list
                 (copy-to-list (left-branch tree)
                               (cons (entry tree)
                                     (copy-to-list (right-branch tree)
                                                   result-list))))))
    (copy-to-list tree '())))

; a) I'm guessing this is a trick question, as, as far as I can tell, they do.
;    If you try it on any of the tree examples, you get (1 3 5 7 9 11)

(defparameter +tree-example-1+ (make-tree 7
                                          (make-tree 3
                                                     (make-tree 1
                                                                nil
                                                                nil)
                                                     (make-tree 5
                                                                nil
                                                                nil))
                                          (make-tree 9 nil (make-tree 11
                                                                      nil
                                                                      nil))))

(defparameter +tree-example-2+ (make-tree 3
                                          (make-tree 1
                                                     nil
                                                     nil)
                                          (make-tree 7
                                                     (make-tree 5
                                                                nil
                                                                nil)
                                                     (make-tree 9
                                                                nil
                                                                (make-tree 11
                                                                           nil
                                                                           nil)))))

(defparameter +tree-example-3+ (make-tree 5
                                          (make-tree 3
                                                     (make-tree 1
                                                                nil
                                                                nil)
                                                     nil)
                                          (make-tree 9
                                                     (make-tree 7 nil nil)
                                                     (make-tree 11 nil nil))))

; b) a is slower because of the append operation. I think both grow O(log n), however.

; Exercise 2.64

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun quotient (n m)
  (floor (/ n m)))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a) Successively splits the 'elts' around a pivot element, which is roughly
;    the centre of the list. This becomes the entry of a new sub-tree, the
;    left and right subtrees of this being formed by recursively calling the same
;    function on them.

; b) The depth of recursion is log n. But for each level there are two recursive
;    calls. ... I vaguely remember that this can be solved with the master method.

;    TODO: look up master method and solve this

;    I think, however, that it is n number of steps.


; Exercise 2.65

(defun union-tree-set (set1 set2)
  (let* ((list1 (tree->list-2 set1))
         (list2 (tree->list-2 set2))
         (combined (union-ordered-set list1 list2)))
    (list->tree combined)))

; tree->list-2 is O(log n). union-ordered-set is O(n). The growth of the function
; is still O(n).

(defun intersection-tree-set (set1 set2)
  (let* ((list1 (tree->list-2 set1))
         (list2 (tree->list-2 set2))
         (intersection (intersection-of-ordered-set list1 list2)))
    (list->tree combined)))

; same as above

; Exercise 2.66

(defun lookup (given-key set-of-records)
  (if (null set-of-records)
      nil
      (let* ((record (entry set-of-records))
             (record-key (key record)))
        ((= given-key record-key) record)
        ((> given-key record-key) (lookup given-key (right-branch set-of-records)))
        ((< given-key record-key) (lookup given-key (left-branch set-of-records))))))
