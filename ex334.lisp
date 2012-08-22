; Exercise 3-3-4

; for make-queue, etc.
(load "ex332.lisp")


(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s)
  (car s))

(defun segment-queue (s)
  (cdr s))

(defun make-agenda ()
  (list 0))

(defun current-time (agenda) (car agenda))

(defun set-current-time! (agenda time)
  (set-car! agenda time))

(defun segments (agenda)
  (cdr agenda))

(defun set-segments! (agenda segments)
  (set-cdr! agenda segments))

(defun first-segment (agenda)
  (car (segments agenda)))

(defun rest-segments (agenda)
  (cdr (segments agenda)))

(defun empty-agenda? (agenda)
  (null (segments agenda)))

(defun add-to-agenda! (time action agenda)
  (labels ((belongs-before? (segments)
             (or (null segments)
                 (< time (segment-time (car segments)))))
           (make-new-time-segment (time action)
             (let ((q (make-queue)))
               (insert-queue! q action)
               (make-time-segment time q)))
           (add-to-segments! (segments)
             (if (= (segment-time (car segments)) time)
                 (insert-queue! (segment-queue (car segments))
                                action)
                 (let ((rest (cdr segments)))
                   (if (belongs-before? rest)
                       (set-cdr!
                        segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
                       (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments!
           agenda
           (cons (make-new-time-segment time action)
                 segments))
          (add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(defparameter *the-agenda* (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)

(defun make-wire ()
  (let ((signal-value 0) (action-procedures '()))
    (labels ((set-my-signal! (new-value)
               (if (not (= signal-value new-value))
                   (let nil
                     (setf signal-value new-value)
                     (call-each action-procedures))
                   'done))
             (accept-action-procedure! (proc)
               (setf action-procedures (cons proc action-procedures))
               (funcall proc))
             (dispatch (m)
               (case m
                 (get-signal signal-value)
                 (set-signal! #'set-my-signal!)
                 (add-action! #'accept-action-procedure!)
                 (t (error "Unknown operation -- wire ~a" m)))))
      #'dispatch)))

(defun call-each (procedures)
  (mapcar #'funcall procedures))

(defun get-signal (wire)
  (funcall wire 'get-signal))

(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))

(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))


(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time *the-agenda*))
                  action
                  *the-agenda*))

(defun propagate ()
  (if (empty-agenda? *the-agenda*)
      'done
      (let ((first-item (first-agenda-item *the-agenda*)))
        (funcall first-item)
        (remove-first-agenda-item! *the-agenda*)
        (propagate))))

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (format t "~a ~a  New-value = ~a ~%"
                         name (current-time *the-agenda*) (get-signal wire)))))

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defun inverter (input output)
  (labels ((invert-input ()
             (let ((new-value (logical-not (get-signal input))))
               (after-delay inverter-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! input #'invert-input)
    'ok))

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error "Invalid signal ~a" s))))

(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
             (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
               (after-delay and-gate-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! a1 #'and-action-procedure)
    (add-action! a2 #'and-action-procedure)
    'ok))

(defun logical-and (a b)
  (cond ((not (and (or (= a 1) (= a 0))
                   (or (= b 1) (= b 0))))
         (error "logical-and invalid signal ~a ~a" a b))
        ((= (+ a b) 2) 1)
        (t 0)))

;; (defparameter input-1 (make-wire))
;; (defparameter input-2 (make-wire))
;; (defparameter sum (make-wire))
;; (defparameter carry (make-wire))

;; (probe 'sum sum)
;; ; sum 0  New-value = 0

;; (probe 'carry carry)
;; ; carry 0  New-value = 0

;; (half-adder input-1 input-2 sum carry)

;; (set-signal! input-1 1)

;; (propagate)
;; ; sum 8  New-value = 1

;; (set-signal! input-2 1)

;; (propagate)
;; ; carry 11  New-value = 1
;; ; sum 16  New-value = 0

; Exercise 3.28

(defun or-gate (a1 a2 output)
  (labels ((or-gate-procedure ()
             (let ((new-value (logical-or (get-signal a1)
                                          (get-signal a2))))
               (after-delay or-gate-delay
                            (lambda ()
                              (set-signal! output new-value))))))
    (add-action! a1 #'or-gate-procedure)
    (add-action! a2 #'or-gate-procedure)
    'ok))

(defun logical-or (a b)
    (cond ((not (and (or (= a 1) (= a 0))
                   (or (= b 1) (= b 0))))
           (error "logical-or invalid signal ~a ~a" a b))
          ((= (+ a b) 1) 1)
          (t 0)))

; Exercise 3.29

(defun compound-or-gate (a1 a2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (n-both (make-wire)))
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 n-both)
    (inverter n-both output)))

;; Should be one and-gate + 2 inverter delays (either of the input inverters
;; or both simultaneously, plus the output inverter).


; Exercise 3.30

(defun ripple-carry-adder (as bs ss c-in)
  (unless (null as)
    (let ((c-out (make-wire)))
      (full-adder (car as) (car bs) c-in (car ss) c-out)
      (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-out))))

; let   A = and-gate delay
;       O = or-gate delay
;       I = inverter delay

; A half adder has    A + 2I

; A full adder has two half adders plus an or gate, so
; 
; 2 (A + 2I) + O
; 2A + 4I + O

; A ripple carry adder consists of n full-adders:   n (2A + 4I + O)

(defun collect-n-applications (n f)
  (if (= n 0)
      '()
      (cons (funcall f) (collect-n-applications (1- n) f))))

;; (defparameter n-bits 4)
;; (defparameter input-as (collect-n-applications n-bits #'make-wire))
;; (defparameter input-bs (collect-n-applications n-bits #'make-wire))
;; (defparameter outputs (collect-n-applications n-bits #'make-wire))
;; (defparameter c-in (make-wire))


;; ; commence the probing
;; (let ((n 1)
;;       (outputs outputs))
;;   (collect-n-applications n-bits (lambda ()
;;                                    (probe n (car outputs))
;;                                    (incf n)
;;                                    (setf outputs (cdr outputs)))))

;; (defparameter adder (ripple-carry-adder input-as input-bs outputs c-in))

;; (set-signal! (car input-as) 1)
;; (set-signal! (car input-bs) 1)
;; (set-signal! (nth 1 input-as) 1)

;; CL-USER> (propagate)
;; 1 8  New-value = 1 
;; 2 8  New-value = 1 
;; 1 16  New-value = 0 
;; 2 32  New-value = 0 
;; 3 48  New-value = 1 


; Exercise 3.31

;; To initialize the output signal given the inputs. e.g. if you attach an
;; inverter to a 0 input, the output should be 1. This would not happen if the
;; procedure was not run as soon as it is attached (instead of just when it
;; changes).


;; (probe 'sum sum)

;; (probe 'carry carry)

;; (half-adder input-1 input-2 sum carry)

;; (set-signal! input-1 1)

;; (propagate)   <- nothing happens because inverter's output is 0 (should be initialized to 1)

;; (set-signal! input-2 1)

;; (propagate)   <- at this point signals from A and B have propagated through the system
;;                  setting all the outputs correctly, so it appears right
;; ; carry 11  New-value = 1 
;; ; sum 16  New-value = 0





; Exercise 3.32

;; (defparameter a1 (make-wire))
;; (defparameter a2 (make-wire))

;; (defparameter out (make-wire))

;; (defparameter gate (and-gate a1 a2 out))

;; (probe 'out out)

;; (set-signal! a2 1)

;; (propagate)

;; (set-signal! a1 0)
;; (set-signal! a2 1)

;; (propagate)


; Each time one of the input wires changes it adds an event to change the
; output wire's value.

; Initially: a1 = 0, a2 = 1, output = 0

; Change a1->1    (now there is an event added to change output->1)
; Change a2->0    (now there is an event added to change output->0)

; In the case of a queue, these are executed in order, so the final value of
; the output is 0, as it should be.

; In the case of a stack (list), these are executed in reverse order, so the
; final value of the output is 1, which is not what it should be given the
; inputs.

