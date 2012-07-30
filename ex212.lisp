; Exercises for 2.1.2


; Exercise 2.2

(defun make-point (x y)
  (cons x y))

(defun x-point (p)
  (car p))

(defun y-point (p)
  (cdr p))

(defun make-segment (a b)
  (cons a b))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

(defun print-point (p)
  (format t "(~a,~a)~%" (x-point p) (y-point p)))

(defun average (&rest args)
  (/ (apply #'+ args) (length args)))

(defun midpoint-segment (s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

(defparameter *test-segment* (make-segment (make-point 1 1) (make-point 9 9)))

;; CL-USER> (print-point (midpoint-segment *test-segment*))
;; (5,5)

; Exercise 2.3

(defun make-rectangle (a b)
  "Rectangle is defined by two points, opposite corners."
  (cons a b))

(defun start-rect (rect)
  (car rect))

(defun end-rect (rect)
  (cdr rect))

(defun x-points (rect)
  (list (x-point (start-rect rect)) (x-point (end-rect rect))))

(defun y-points (rect)
  (list (y-point (start-rect rect)) (y-point (end-rect rect))))

;;

(defun left (rect)
  (apply #'min (x-points rect)))

(defun top (rect)
  (apply #'min (y-points rect)))

(defun right (rect)
  (apply #'max (x-points rect)))

(defun bottom (rect)
  (apply #'max (y-points rect)))

;;;;

(defun height (rect)
  (- (bottom rect) (top rect)))

(defun width (rect)
  (- (right rect) (left rect)))

;;

(defun perimeter (rect)
  (* (+ (height rect) (width rect)) 2))

(defun area (rect)
  (* (height rect) (width rect)))

; b)

;; I'm not really sure what other representation you can use which isn't kind of dumb ...

(defun make-rectangle-2 (top-left height width)
  (cons top-left (cons height width)))

(defun top-left-2 (rect)
  (car rect))

; now assuming these are called 'width' and 'height' instead, perimeter and area still work.

(defun height-2 (rect)
  (car (cdr rect)))

(defun width-2 (rect)
  (cdr (cdr rect)))
