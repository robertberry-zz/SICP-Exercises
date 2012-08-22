; Answers for 3-3-3

(defun set-cdr! (x y)
  (setf (cdr x) y))

(defun set-car! (x y)
  (setf (car x) y))

(defun lookup (key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
        (cdr record)
        nil)))

(defun my-assoc (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (t (my-assoc key (cdr records)))))

(defun insert! (key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))
    'ok))

(defun make-table ()
  '(*table*))

(defun lookup-2d (key-1 key-2 table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              nil))
        nil)))

(defun insert-2d! (key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))
    'ok))

(defun make-table-proc ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
               (let ((subtable (my-assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (my-assoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (my-assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (my-assoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                     (set-cdr! local-table
                               (cons (list key-1
                                           (cons key-2 value))
                                     (cdr local-table))))
                 'ok))
             (dispatch (m)
               (case m
                 (lookup-proc #'lookup)
                 (insert-proc! #'insert!)
                 (t (error "Unknown operation -- TABLE ~a" m)))))
      #'dispatch)))

; Exercise 3.24

(defun make-table-proc-2 (same-key?)
  (let ((local-table (list '*table*)))
    (labels ((my-assoc (key records)
               (cond ((null records) nil)
                     ((funcall same-key? key (caar records)) (car records))
                     (t (my-assoc key (cdr records)))))
             (lookup (key-1 key-2)
               (let ((subtable (my-assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (my-assoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (my-assoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (my-assoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                     (set-cdr! local-table
                               (cons (list key-1
                                           (cons key-2 value))
                                     (cdr local-table))))
                 'ok))
             (dispatch (m)
               (case m
                 (lookup-proc #'lookup)
                 (insert-proc! #'insert!)
                 (t (error "Unknown operation -- TABLE ~a" m)))))
      #'dispatch)))

; Exercise 3.25

(defun make-table-3 (&optional (same-key? #'equal))
  (let ((local-table (list '*table*)))
    (labels ((my-assoc (key records)
               (cond ((null records) nil)
                     ((funcall same-key? key (caar records)) (car records))
                     (t (my-assoc key (cdr records)))))
             (lookup (keys)
               (labels ((iter (table keys)
                          (cond ((null table) nil)
                                ((null keys) (cdr table))
                                (t (iter (my-assoc (car keys) (cdr table))
                                         (cdr keys))))))
                 (iter local-table keys)))
             (insert! (keys value)
               (labels ((iter (keys table)
                          (if (null keys)
                              (set-cdr! table value)
                              (let ((subtable (my-assoc (car keys) (cdr table))))
                                (if subtable
                                    (iter (cdr keys) subtable)
                                    (append-item! keys value table)))))
                        (append-item! (keys value table)
                          (if (null keys)
                              (set-cdr! table value)
                              (let ((new-record (list (car keys))))
                                (set-cdr! table (cons new-record (cdr table)))
                                (append-item! (cdr keys) value new-record)))))
                 (iter keys local-table)))
             (dispatch (m)
               (case m
                 (lookup #'lookup)
                 (insert! #'insert!)
                 (t (error "Unknown operation -- TABLE ~a" m)))))
      #'dispatch)))

(defun insert-table! (table keys value)
  (funcall (funcall table 'insert!) keys value))

(defun lookup-table (table keys)
  (funcall (funcall table 'lookup) keys))

; Exercise 3.26

;; Even better, I'll implement it.

(defun symbol> (a b)
  (not (null (string> (symbol-name a)
                      (symbol-name b)))))

(defun make-tree (&optional (after? #'symbol>) (key #'identity) (equal? #'equal))
  (let ((tree '()))
    (labels ((make-node (value left right)
               (list value left right))
             (left (node)
               (cadr node))
             (right (node)
               (caddr node))
             (set-left! (node tree)
               (setf (cadr node) tree))
             (set-right! (node tree)
               (setf (caddr node) tree))
             (value (node)
               (car node))
             (insert! (x)
               (let ((new-node (make-node x '() '())))
                 (labels ((iter (node)
                            (if (funcall after? (funcall key x) (funcall key (value node)))
                                (if (null (right node))
                                    (set-right! node new-node)
                                    (iter (right node)))
                                (if (null (left node))
                                    (set-left! node new-node)
                                    (iter (left node))))))
                   (if (null tree)
                       (setf tree new-node)
                       (iter tree)))))
             (lookup (x)
               (labels ((iter (node)
                          (if (null node)
                              nil
                              (let ((node-val (value node)))
                                (if (funcall equal? x (funcall key node-val))
                                    node-val
                                    (iter (if (funcall after? x (funcall key node-val))
                                              (right node)
                                              (left node))))))))
                 (iter tree)))
             (dispatch (m)
               (case m
                 (insert! #'insert!)
                 (lookup #'lookup)
                 (t (error "Bad message for tree ~a" m)))))
      #'dispatch)))

(defun tree-insert! (tree a)
  (funcall (funcall tree 'insert!) a))

(defun tree-lookup (tree a)
  (funcall (funcall tree 'lookup) a))

(defun make-table-4 (&optional (same-key? #'equal) (key-after? #'symbol>))
  (let ((local-table (cons '*table* (make-tree key-after? #'car same-key?))))
    (labels ((my-assoc (key records)
               (tree-lookup records key))
             (lookup (keys)
               (labels ((iter (table keys)
                          (cond ((null table) nil)
                                ((null keys) (cdr table))
                                (t (iter (my-assoc (car keys) (cdr table))
                                         (cdr keys))))))
                 (iter local-table keys)))
             (insert! (keys value)
               (labels ((iter (keys table)
                          (if (null keys)
                              (set-cdr! table value)
                              (let ((subtable (my-assoc (car keys) (cdr table))))
                                (if subtable
                                    (iter (cdr keys) subtable)
                                    (append-item! keys value table)))))
                        (append-item! (keys value table)
                          (if (null keys)
                              (set-cdr! table value)
                              (let ((new-record (list (car keys)))
                                    (new-subtree (make-tree key-after? #'car same-key?)))
                                (tree-insert! new-subtree new-record)
                                (set-cdr! table new-subtree)
                                (append-item! (cdr keys) value new-record)))))
                 (iter keys local-table)))
             (dispatch (m)
               (case m
                 (lookup #'lookup)
                 (insert! #'insert!)
                 (t (error "Unknown operation -- TABLE ~a" m)))))
      #'dispatch)))

; Exercise 3.27

; a)

;; Ignoring all the calls to the table functions except lookup. They are all bound to global env.

;;              +------------------------------------------------------------------+
;;  global  --> | memo-fib: ----------------------------------+
;;  env         | memoize:                                    |
;;              +-------|-------------------------------------+--------------------+
;;                      v                ^  ^                 |
;;                  .--- ---.            |  |                 |
;;                  | 0 | 0-+------------+  |             +---+---+
;;                  `-|-^---'   +-----------+             | 0 | 0 +------------+
;;                    |         |                         +-|-+---+            |
;;                    v         |                           |                  |
;;             parameters: f    |                           v                  |
;;             body: ...        |                  parameters: n               |
;;                              |                  body: (cond ... )           |
;;     (memoize (lambda (n)) .. |                                              |
;;           +------------------+--+               (let ((table (make-table))) v
;;      E1-->| f: (lambda (n) ...  |<--------+           +----------------------+
;;           +---------------------+         +-----------+ table: (new table)   |<--- E2
;;                                                       +-----^---------^------+
;;                                                             |         |
;;                                                             |    +----+---+
;;                                                             |    | x: 3   |<------ E3
;;                                                             |    +--------+ (memo-fib 3)
;;                                                             |
;;                                                             |    +--------+
;;                                                             +----+ x: 2   |<------ E6
;;                                                             |    +--------+ (memo-fib (- n 1))
;;                                                             |
;;                                                             |    +--------+
;;                                                             +----+ x: 1   |<------ E9
;;                                                             |    +--------+ (memo-fib (- n 1))
;;                                                             |
;;                                                             |    +--------+
;;                                                             +----+ x: 0   |<------ E12
;;                                                             |    +--------+ (memo-fib (- n 2))
;;                                                             |
;;                                                             |    +--------+
;;                                                             +----+ x: 1   |<------ E15
;;                                                                  +--------+ (memo-fib (- n 2))

;; ---------------------------------------------------+
;;  Lookup                    make-table              |
;;  insert!                                           | <--  global env
;; ----------^----------^-----------^-----------------+
;;  (f x)    |          |           |    (lookup x table)
;;      +----+----+     |     +-----+----+
;; E5 ->| n: 3    |     |     | key: 3   |<------ E4
;;      +---------+     |     +----------+
;;  (f x)               |                (lookup x table)
;;      +---------+     |     +----------+
;; E8 ->| n: 2    +-----+-----| key: 2   |<------ E7
;;      +---------+     |     +----------+
;;  (f x)               |                (lookup x table)
;;      +---------+     |     +----------+
;; E11->| n: 1    +-----+-----| key: 1   |<------ E10
;;      +---------+     |     +----------+
;;  (f x)               |                (lookup x table)
;;      +---------+     |     +----------+
;; E14->| n: 0    +-----+-----| key: 0   |<------ E13
;;      +---------+     |     +----------+
;;                      |                (lookup x table)
;;                      |     +----------+
;;                      +-----| key: 1   |<------ E16
;;                            +----------+



; b)

; Because it does not have to recurse all the way down the structure. If a
; value has previously been computed it will be immediately returned, whereas
; in the original recursive procedure, the two recursive calls redo a lot of
; work.

; c)

; No, because fib's recursions refer to the 'fib' procedure itself, so it would
; only use the memoized procedure on the first level of recursion.