; 2.2.2

; Exercise 2.24

;; Goddammit D:

; a)

;  (1 (2 (3 4)))                       
                                       
; b)  _________     ________           
;     | . | ._|___\ | . |  /|          
;     |_|_|___|   / |_|_|/__|          
;       |             |                
;                    \|/               
;       1           ________      ________ 
;                   | . | ._|___\ | . |  /|
;                   |_|_|___|   / |_|_|/__|
;                     |             |      
;                                   |             
;                     2            \|/          
;                                 ________      ________ 
;                                 | . | ._|___\ | . |  /|
;                                 |_|_|___|   / |_|_|/__|
;                                   |             |     
;                                                       
;                                   3             4     
;                                                       

; c)
;                    (1 (2 (3 4)))
;                        /\
;                       /  \
;                      1   (2 (3 4)
;                          / \
;                         /   \
;                        2     (3 4)
;                               /  \
;                              3    4
;

; Exercise 2.25

; a) (1 3 (5 7) 9)
;
;    (car (cdr (car (cdr (cdr x)))))

; b) ((7))
;
;    (car (car x))

; c) (1 (2 (3 (4 (5 (6 7))))))
;
;    (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x))))))))))))

; wtf that was mean :(

; Exercise 2.26

; a)

; (1 2 3 4 5 6)

; b)

; ((1 2 3) 4 5 6)

; c)

; ((1 2 3) (4 5 6))

; Exercise 2.27

(defun deep-reverse (x)
  (labels ((iter (x acc)
             (cond ((null x) acc)
                   ((atom x) x)
                   (t (iter (cdr x) (cons (iter (car x) '()) acc))))))
    (iter x '())))

;; Pretty proud of that one actually ... 

; Exercise 2.28

(defun fringe (x)
  (cond ((null x) '())
        ((atom x) (list x))
        (t (append (fringe (car x)) (fringe (cdr x))))))

; Exercise 2.29

;; TODO