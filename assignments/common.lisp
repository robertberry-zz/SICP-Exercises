; Functions to emulate the SICP Scheme

(defmacro scheme-alias (scheme-name cl-name)
  `(defun ,scheme-name (&rest args)
     (apply #',cl-name args)))

(defun newline ()
  (format t "~%"))


(defun read-from-keyboard ()
  (let ((char (read-char)))
    (read-char) ; lose the new line
    char))

(scheme-alias read-from-keyboard read-char)
(scheme-alias eq? eq)
