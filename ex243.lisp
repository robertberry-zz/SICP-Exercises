; Answers for 2-4-3

; NOTE: Much of this chapter is untested, as we have not yet been supplied
; with the 'put and 'get operations. todo: return to this later to test.

; Exercise 2.73

(defun deriv (exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (t (funcall (get 'deriv (operator exp))
                    (operands exp)
                    var))))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

; a)

; They're primitives of the language, not lists. We could modify the code to
; use our own 'types' for numbers and variables, though, representing them as
; lists with the first element being 'number and 'variable, for example.

; b)

(defun install-basic-deriv-rules ()
  (labels ((deriv-sum (exp var)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
           (deriv-product (exp var)
             (make-sum
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp)))))
    (put 'deriv '+ #'deriv-sum)
    (put 'deriv '* #'deriv-product)))

; c)

(defun install-extra-deriv-rules ()
  (labels ((deriv-exponentiation (exp var)
             (make-product
              (make-product
               (exponent exp)
               (make-exponentiation (base exp) (1- (exopnent exp))))
              (deriv (base exp) var))))
    (put 'deriv '** #'deriv-exopnentiation)))

; d)

; Weird question ... +, for example, is now being dispatched based on the type
; of the arguments, so you would have to provide a 'deriv -typed argument to
; it, which would assumedly ..

; Seems like a dumb representation to me! D: todo: think more about this


; Exercise 2.74

; a)

; The file must be supplied as a pair, whose first element is its type code as
; a symbol.

; The division must then provide the function for extracting the record from
; this file type, and install it with (put 'get-record 'file-type-here
; #'func-here)

(defun personnel-file-type (personnel-file)
  (car personnel-file))

(defun get-record (personnel-file employee-id)
  (funcall (get 'get-record (personnel-file-type personnel-file))
           personnel-file employee-id))


; b)

; The record must be a pair, whose first element is its type code as a symbol.

; The rest is up to the division - they simply have to supply the
; implementation for 'get-salary for that type (as with 'get-record above).

(defun record-type (record)
  (car record))

(defun get-salary (record)
  (funcall (get 'get-salary (record-type record)) record))

; c)

(defun find-employee-record (employee-id personnel-files)
  (if (null personnel-files)
      nil
      (let ((record (get-record (car personnel-files) employee-id)))
        (if (null record)
            (find-employee-record employee-id (cdr personnel-files))
            record))))

; d)

; The new division must supply their personnel files as pairs, the first
; element of which contains the type code unique to that division, the rest of
; which is up to the division.

; They must supply a function to extract a record for a given employee-id from
; one of their personnel files, and then install it under that type code. (see a).

; This function should return the record as a pair, whose first element is a
; type code unique to the division. The rest is up to the division.

; They must then supply functions to operate on this record type and install
; them for the type (e.g. 'get-salary', as in b).


; Exercise 2.75

(defun make-from-mag-ang (mag ang)
  (labels ((dispatch (op)
             (case op
               (real-part (* mag (cos ang)))
               (imag-part (* mag (sin ang)))
               (magnitude mag)
               (angle ang)
               (t (error "Unknown op - MAKE-FROM-MAG-ANG")))))
    #'dispatch))

; Exercise 2.76

; a) Which organization would be most
;    appropriate for a system in which new types must often be added?

; Either data-directed or message-passing is good for this. The user does not
; need to edit the core code to implement new types. Data-directed makes it
; easier to add multiple constructors, whereas message-passing simplifies the
; installation code.

; NB Our implementation of message-passing does not supply generic operations
; for multiple arguments.

; b) Which would be most appropriate for a system in which new
;    operations must often be added?

; It's probably actually easiest for the explicit dispatch, as the new
; operation is defined all in one place, explicitly providing operations for
; each of the types. In message-passing and data-directed styles, the new
; operations would have to be defined in many different places (but perhaps it
; is most appropriate that whoever is responsible for a given library defines
; the operations anyway).

