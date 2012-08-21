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

; Exercise 3.26

;; todo ...

