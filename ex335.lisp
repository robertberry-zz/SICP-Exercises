; Answers for 3-3-5

(defun adder (a1 a2 sum)
  (labels ((process-new-value ()
             (cond ((and (has-value? a1) (has-value? a2))
                    (set-value! sum
                                (+ (get-value a1) (get-value a2))
                                #'me))
                   ((and (has-value? a1) (has-value? sum))
                    (set-value! a2
                                (- (get-value sum) (get-value a1))
                                #'me))
                   ((and (has-value? a2) (has-value? sum))
                    (set-value! a1
                                (- (get-value sum) (get-value a2))
                                #'me))))
           (process-forget-value ()
             (forget-value! sum #'me)
             (forget-value! a1 #'me)
             (forget-value! a2 #'me)
             (process-new-value))
           (me (request)
             (case request
               (I-have-a-value (process-new-value))
               (I-lost-my-value (process-forget-value))
               (otherwise (error "Unknown request ~a" request)))))
    (connect a1 #'me)
    (connect a2 #'me)
    (connect sum #'me)
    #'me))

(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-value))

(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value))


                                
(defun multiplier (m1 m2 product)
  (labels ((process-new-value ()
             (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                        (and (has-value? m2) (= (get-value m2) 0)))
                    (set-value! product 0 #'me))
                   ((and (has-value? m1) (has-value? m2))
                    (set-value! product
                                (* (get-value m1) (get-value m2))
                                #'me))
                   ((and (has-value? product) (has-value? m1))
                    (set-value! m2
                                (/ (get-value product) (get-value m1))
                                #'me))
                   ((and (has-value? product) (has-value? m2))
                    (set-value! m1
                                (/ (get-value product) (get-value m2))
                                #'me))))
           (process-forget-value ()
             (forget-value! product #'me)
             (forget-value! m1 #'me)
             (forget-value! m2 #'me)
             (process-new-value))
           (me (request)
             (case request
               (I-have-a-value (process-new-value))
               (I-lost-my-value (process-forget-value))
               (otherwise (error "Unknown request ~a" request)))))
    (connect m1 #'me)
    (connect m2 #'me)
    (connect product #'me)
    #'me))


(defun constant (value connector)
  (labels ((me (request)
             (error "Unknown request -- const ~a" request)))
    (connect connector #'me)
    (set-value! connector value #'me)
    #'me))


(defun probe (name connector)
  (labels ((print-probe (value)
             (format t "Probe: ~a = ~a ~%" name value))
           (process-new-value ()
             (print-probe (get-value connector)))
           (process-forget-value ()
             (print-probe "?"))
           (me (request)
             (case request
               (I-have-a-value (process-new-value))
               (I-lost-my-value (process-forget-value))
               (otherwise (error "Unknown request ~a" request)))))
    (connect connector #'me)
    #'me))


(defun make-connector ()
  (let ((value nil)
        (informant nil)
        (constraints '()))
    (labels ((set-my-value (newval setter)
               (cond ((not (has-value? #'me))
                      (setf value newval)
                      (setf informant setter)
                      (for-each-except setter
                                       #'inform-about-value
                                       constraints))
                     ((not (= value newval))
                      (error "Contradiction ~a ~a" value newval))
                     (t 'ignored)))
             (forget-my-value (retractor)
               (if (eq retractor informant)
                   (let nil
                     (setf informant nil)
                     (for-each-except retractor
                                      #'inform-about-no-value
                                      constraints))
                   'ignored))
             (connect (new-constraint)
               (if (not (member new-constraint constraints))
                   (setf constraints
                         (cons new-constraint constraints)))
               (if (has-value? #'me)
                   (inform-about-value new-constraint))
               'done)
             (me (request)
               (case request
                 (has-value? (not (null informant)))
                 (value value)
                 (set-value! #'set-my-value)
                 (forget #'forget-my-value)
                 (connect #'connect)
                 (otherwise (error "Unknown operation -- CONNECTOR ~a" request)))))
      #'me)))

(defun for-each-except (exception procedure list)
  (labels ((iter (items)
             (cond ((null items) 'done)
                   ((eq (car items) exception) (iter (cdr items)))
                   (t (funcall procedure (car items))
                      (iter (cdr items))))))
    (iter list)))
                           

(defun has-value? (connector)
  (funcall connector 'has-value?))

(defun get-value (connector)
  (funcall connector 'value))

(defun set-value! (connector new-value informant)
  (funcall (funcall connector 'set-value!) new-value informant))

(defun forget-value! (connector retractor)
  (funcall (funcall connector 'forget) retractor))

(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))

(defun celsius-farenheit-converter (c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))



(defparameter C (make-connector))
(defparameter F (make-connector))
(celsius-farenheit-converter C F)




(probe "Celsius temp" C)
(probe "Farenheit temp" F)


;; CL-USER> (set-value! C 25 'user)
;; Probe: Celsius temp = 25 
;; Probe: Farenheit temp = 77 
;; DONE
;; CL-USER> (forget-value! C 'user)
;; Probe: Celsius temp = ? 
;; Probe: Farenheit temp = ? 
;; DONE
;; CL-USER> (set-value! F 212 'user)
;; Probe: Farenheit temp = 212 
;; Probe: Celsius temp = 100 
;; DONE



; Exercise 3.33

(defun averager (a b ave)
  (let ((a+b (make-connector))
        (two (make-connector)))
    (adder a b a+b)
    (constant 2 two)
    (multiplier two ave a+b)))

(defparameter x (make-connector))
(defparameter y (make-connector))
(defparameter ave-xy (make-connector))

(averager x y ave-xy)


(probe 'x x)
(probe 'y y)
(probe 'average ave-xy)



;; CL-USER> (set-value! x 1 'user)
;; Probe: X = 1 
;; DONE
;; CL-USER> (set-value! y 5 'user)
;; Probe: Y = 5 
;; Probe: AVERAGE = 3 
;; DONE
;; CL-USER> (forget-value! x 'user)
;; Probe: X = ? 
;; Probe: AVERAGE = ? 
;; DONE
;; CL-USER> (set-value! ave-xy 10 'user)
;; Probe: AVERAGE = 10 
;; Probe: X = 15 
;; DONE


; Exercise 3.34

;; It won't work backwards (can't figure out square roots). If you set b to
;; some value and a is not set, it will not try to set a, as it has neither
;; multiplicand. If a is set, then it will simply set a to b/a, which is not
;; the square root (unless a already happens to be the square root).


; Exercise 3.35

(defun square (x)
  (* x x))

(defun squarer (a b)
  (labels ((process-new-value ()
             (if (has-value? b)
                 (if (< (get-value b) 0)
                     (error "square less than 0 -- squarer ~a" (get-value b))
                     (set-value! a (sqrt (get-value b)) #'me))
                 (if (has-value? a)
                     (set-value! b (square (get-value a)) #'me))))
           (process-forget-value ()
             (forget-value! a #'me)
             (forget-value! b #'me)
             (process-new-value))
           (me (request)
             (case request
               (I-have-a-value (process-new-value))
               (I-lost-my-value (process-forget-value))
               (otherwise (error "Unknown request ~a" request)))))
    (connect a #'me)
    (connect b #'me)
    #'me))

(defparameter s (make-connector))
(defparameter u (make-connector))

(probe 's s)
(probe 'u u)

(squarer s u)

;; CL-USER> (set-value! s 5 'user)
;; Probe: U = 25 
;; Probe: S = 5 
;; DONE
;; CL-USER> (forget-value! s 'user)
;; Probe: U = ? 
;; Probe: S = ? 
;; DONE
;; CL-USER> (set-value! u 49 'user)
;; Probe: S = 7.0 
;; Probe: U = 49 
;; DONE


; Exercise 3.36

;;              +------------------------------------------------------------------+
;;  global  --> | make-connector
;;  env         | a
;;              +-|--------------------------^-------------------------------------+
;;                v                          |            ^    (make-connector)
;;           .--- ---.                       |         +--|--+
;;           | 0 | 0-+--------+              |         |     | <-- E1 (empty)
;;           `-|-^---'        |              |         +-----+
;;             |              |              |            ^
;;             v              |              |            |     (let ((value false ... )
;;      parameters: request   |              |         +--+--------------------+
;;      body: (cond ((eq? ... |              |         | value: false          |
;;                            +----------------------->| informant: false      |<-- E2
;;    ((...) new-value informant)            |         | constraints: '()      |
;;        +-------------------+              |         | set-my-value          |
;;        | newval: 10        |              |         | forget-my-value       |
;;  E7--->| setter: 'user     |              |         | connect               |
;;        +-------------------+              |         | me                    |
;;   +---------------------------------+     |         +-----------------------+
;;   | exception: 'user                |     |           ^
;;   | procedure: inform-about-value   +-----+           |
;;   | list: constraints (from E2)     |                 |  ((connector 'set-value!) ..)
;;   +--------------^------------------+               +-+---------------------+
;;                  |                                  | request: 'set-value!  |<-- E6
;;                 E8                                  +-----------------------+

;;     global env
;; -----------------------------------------------------------------------------------
;;             set-value!
;;             b
;; ------------|----------------------------------------------------------------------
;;             v                       ^                  ^    (make-connector)
;;           .--- ---.                 |               +--|--+
;;           | 0 | 0-+--------+        |               |     | <-- E3 (empty)
;;           `-|-^---'        |        |               +-----+
;;             |              |        |                  ^
;;             v              |        |                  |     (let ((value false ... )
;;      parameters: request   |        |               +--+--------------------+
;;      body: (cond ((eq? ... |        |               | value: false          |
;;                            +----------------------->| informant: false      |<-- E4
;;        (set-value! ... )            |               | constraints: '()      |
;;       +-------------------+         |               | set-my-value          |
;;       | connector: a      |         |               | forget-my-value       |
;;  E5-->| new-value: 10     +---------+               | connect               |
;;       | informant: 'user  |                         | me                    |
;;       +-------------------+                         +-----------------------+

; Exercise 3.37

(defun c+ (x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(defun c/ (x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(defun cv (n)
  (let ((c (make-connector)))
    (constant n c)
    c))

(defun celsius-farenheit-converter-2 (x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(defparameter C2 (make-connector))
(defparameter F2 (celsius-farenheit-converter-2 C2))

(probe 'Celsius C2)
(probe 'Farenheit F2)


;; CL-USER> (set-value! C2 24 'user)
;; Probe: CELSIUS = 24 
;; Probe: FARENHEIT = 376/5 
;; DONE
;; CL-USER> (forget-value! C2 'user)
;; Probe: CELSIUS = ? 
;; Probe: FARENHEIT = ? 
;; DONE
;; CL-USER> (set-value! F2 125 'user)
;; Probe: FARENHEIT = 125 
;; Probe: CELSIUS = 155/3 
;; DONE
