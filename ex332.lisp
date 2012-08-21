; Answers for 3-3-2

(defun set-cdr! (x y)
  (setf (cdr x) y))

(defun set-car! (x y)
  (setf (car x) y))

(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun set-front-ptr! (queue item)
  (set-car! queue item))

(defun set-rear-ptr! (queue item)
  (set-cdr! queue item))

(defun empty-queue? (queue)
  (null (front-ptr queue)))

(defun make-queue ()
  (cons '() '()))

(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue ~a" queue)
      (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (t
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(defun delete-queue! (queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue ~a" queue))
        (t
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; Exercise 3.21

; Because the queue is represented as a pointer to the front and rear, so we are given
; the print out of a pair containing the queue and the last item.

(defun print-queue (queue)
  (if (empty-queue? queue)
      (format t "queue()")
      (format t "queue~a" (front-ptr queue))))

(defparameter q1 (make-queue))

;; CL-USER> (insert-queue! q1 'a)
;; CL-USER> (print-queue q1)
;; queue(A)
;; CL-USER> (insert-queue! q1 'b)
;; CL-USER> (print-queue q1)
;; queue(A B)
;; CL-USER> (delete-queue! q1)
;; CL-USER> (print-queue q1)
;; queue(B)
;; CL-USER> (delete-queue! q1)
;; CL-USER> (print-queue q1)
;; queue()


; Exercise 3.22

(defun make-queue-2 ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((empty? ()
               (null front-ptr))
             (front ()
               (if (empty?)
                   (error "FRONT called with an empty queue ~a" front-ptr)
                   (car front-ptr)))
             (set-front! (x)
               (setf front-ptr x))
             (set-rear! (x)
               (setf rear-ptr x))
             (insert! (x)
               (let ((new-pair (cons x '())))
                 (cond ((empty?)
                        (set-front! new-pair)
                        (set-rear! new-pair)
                        #'dispatch)
                       (t
                        (set-cdr! rear-ptr new-pair)
                        (set-rear! new-pair)
                        #'dispatch))))
             (delete! ()
               (cond ((empty?)
                      (error "DELETE called with an empty queue ~a" front-ptr))
                     (t
                      (set-front! (cdr front-ptr))
                      #'dispatch)))
             (dispatch (m)
               (case m
                 (empty? #'empty?)
                 (front #'front)
                 (insert! #'insert!)
                 (delete! #'delete!)
                 (otherwise (error "Unknown message ~a" m)))))
      #'dispatch)))

(defun front-queue-2 (queue)
  (funcall (funcall queue 'front)))

(defun empty-queue-2? (queue)
  (funcall (funcall queue 'empty?)))

(defun insert-queue-2! (queue x)
  (funcall (funcall queue 'insert!) x))

(defun delete-queue-2! (queue)
  (funcall (funcall queue 'delete!)))

(defparameter q2 (make-queue-2))

;; CL-USER> (empty-queue-2? q2)
;; NIL
;; CL-USER> (insert-queue-2! q2 'b)
;; #<CLOSURE (LABELS DISPATCH :IN MAKE-QUEUE-2) {C83A125}>
;; CL-USER> (front-queue-2 q2)
;; A
;; CL-USER> (delete-queue-2! q2)
;; #<CLOSURE (LABELS DISPATCH :IN MAKE-QUEUE-2) {C83A125}>
;; CL-USER> (front-queue-2 q2)
;; B
;; CL-USER> (delete-queue-2! q2)
;; NIL
;; CL-USER> (empty-queue-2? q2)
;; T


; Exercise 3.23

;; Borrowing front-ptr, set-front-ptr!, rear-ptr and set-rear-ptr! for this

(defun make-deque ()
  (cons '() '()))

(defun make-deque-item (value next previous)
  (list value next previous))

(defun value-deque-item (deque-item)
  (car deque-item))

(defun next-deque-item (deque-item)
  (cadr deque-item))

(defun previous-deque-item (deque-item)
  (caddr deque-item))

(defun set-deque-item-next! (deque-item next)
  (setf (cadr deque-item) next))

(defun set-deque-item-previous! (deque-item previous)
  (setf (caddr deque-item) previous))

; This to aid garbage collection
(defun cleanup-deque-item (deque-item)
  (set-deque-item-next! deque-item nil)
  (set-deque-item-previous! deque-item nil))

(defun empty-deque? (deque)
  (null (front-ptr deque)))

(defun front-deque (deque)
  (cond ((empty-deque? deque)
         (error "FRONT called on empty deque ~a" deque))
        (t (value-deque-item (front-ptr deque)))))

(defun rear-deque (deque)
  (cond ((empty-deque? deque)
         (error "REAR called on empty deque ~a" deque))
        (t (value-deque-item (rear-ptr deque)))))

(defun front-insert-deque! (deque x)
  (let ((new-item (make-deque-item x (front-ptr deque) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           nil)
          (t
           (set-deque-item-previous! (front-ptr deque) new-item)
           (set-front-ptr! deque new-item)
           nil))))

(defun rear-insert-deque! (deque x)
  (let ((new-item (make-deque-item x nil (rear-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           nil)
          (t
           (set-deque-item-next! (rear-ptr deque) new-item)
           (set-rear-ptr! deque new-item)
           nil))))

(defun front-delete-deque! (deque)
  (cond ((empty-deque? deque)
         (error "DELETE called on empty deque ~a" deque))
        (t
         (let ((new-front (next-deque-item (front-ptr deque))))
           (cleanup-deque-item (front-ptr deque))
           (set-front-ptr! deque new-front)
           (if (null new-front)
               (set-rear-ptr! deque new-front)
               (set-deque-item-previous! new-front nil))
           nil))))

(defun rear-delete-deque! (deque)
  (cond ((empty-deque? deque)
         (error "DELETE called on empty deque ~a" deque))
        (t
         (let ((new-rear (previous-deque-item (rear-ptr deque))))
           (cleanup-deque-item (rear-ptr deque))
           (set-rear-ptr! deque new-rear)
           (if (null new-rear)
               (set-front-ptr! deque new-rear)
               (set-deque-item-next! new-rear nil))
           nil))))

(defun print-deque (deque)
  (labels ((iter (item)
             (when (not (null item))
               (format t " ~a " (value-deque-item item))
               (iter (next-deque-item item)))))
    (format t "Deque: ")
    (unless (empty-deque? deque)
      (iter (front-ptr deque)))))

(defparameter d1 (make-deque))

;; CL-USER> (print-deque d1)
;; Deque: 
;; CL-USER> (front-insert-deque! d1 'a)
;; CL-USER> (print-deque d1)
;; Deque:  A 
;; CL-USER> (front-insert-deque! d1 'b)
;; CL-USER> (print-deque d1)
;; Deque:  B  A 
;; CL-USER> (rear-insert-deque! d1 'c)
;; CL-USER> (print-deque d1)
;; Deque:  B  A  C 
;; CL-USER> (front-insert-deque! d1 'd)
;; CL-USER> (print-deque d1)
;; Deque:  D  B  A  C 
;; CL-USER> (front-delete-deque! d1)
;; CL-USER> (print-deque d1)
;; Deque:  B  A  C 
;; CL-USER> (front-delete-deque! d1)
;; CL-USER> (print-deque d1)
;; Deque:  A  C 
;; CL-USER> (rear-delete-deque! d1)
;; CL-USER> (print-deque d1)
;; Deque:  A 
;; CL-USER> (rear-delete-deque! d1)
;; CL-USER> (print-deque d1)
;; Deque: 
;; CL-USER> (front-insert-deque! d1 'a)
;; CL-USER> (print-deque d1)
;; Deque:  A 
;; CL-USER> (front-deque d1)
;; A
;; CL-USER> (rear-deque d1)
;; A
