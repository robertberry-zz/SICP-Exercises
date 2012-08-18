; Answers for 3-1-2

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun rand ()
  (random 10000))

(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0)
                    (/ trials-passed trials))
                   ((funcall experiment)
                    (iter (- trials-remaining 1) (1+ trials-passed)))
                   (t
                    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))

; Exercise 3.5

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random (+ range 0.0)))))

(defun rect-area (x1 x2 y1 y2)
  (let ((width (abs (- x2 x1)))
        (height (abs (- y2 y1))))
    (* width height)))

(defun estimate-integral (p x1 x2 y1 y2 trials)
  (labels ((experiment ()
             (funcall p
                      (random-in-range x1 x2)
                      (random-in-range y1 y2))))
    (* (rect-area x1 x2 y1 y2)
       (monte-carlo trials #'experiment))))

(defvar pi-estimate)

(defun square (x)
  (* x x))

(defun make-circle-predicate (centre-x centre-y radius)
  (lambda (x y)
    (<= (+ (square (- x centre-x)) (square (- y centre-y)))
        (square radius))))

(let ((x1 0)
      (x2 1)
      (y1 0)
      (y2 1)
      (p (make-circle-predicate 0.5 0.5 0.5)))
  (setf pi-estimate (estimate-integral p x1 x2 y1 y2 10000)))

; Exercise 3.6

(defvar random-init 1)

; As rand-update does not exist in common lisp, making a crappy placeholder,
; so I can test the rest
(defun rand-update (x)
  (+ 1 x))

(let ((x random-init))
  (defun scheme-rand (message)
    (case message
      (generate (setf x (rand-update x)))
      (reset (lambda (new-value)
               (setf x new-value)))
      (otherwise (error "No operation for ~a" message)))))

;; CL-USER> (scheme-rand 'generate)
;; 2
;; CL-USER> (scheme-rand 'generate)
;; 3
;; CL-USER> (scheme-rand 'generate)
;; 4
;; CL-USER> (funcall (scheme-rand 'reset) 10)
;; 10
;; CL-USER> (scheme-rand 'generate)
;; 11
;; CL-USER> (scheme-rand 'generate)
;; 12
