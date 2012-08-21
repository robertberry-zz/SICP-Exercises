; Answers for 3-3-1

(defun set-cdr! (x y)
  (setf (cdr x) y))

(defun set-car! (x y)
  (setf (car x) y))

(defun append! (x y)
  (let nil
    (set-cdr! (last-pair x) y)
    x))

(defun last-pair (x)
  (if (null (cdr x))
      x
      (last-pair (cdr x))))

; Exercise 3.12

(defvar x '(a b))
(defvar y '(c d))
(defvar z (append x y))

; (cdr x) at this point will be '(b), as we used the non-destructive append.

;         +---+---+      +---+---+
;    x -->| * | *-+----->| * | / |
;         +-|-+---+      +-|-+---+
;           v              v
;         +---+          +---+
;         | a |          | b |
;         +---+          +---+
;
;         +---+---+      +---+---+
;    z -->| * | *-+----->| * | * +----+
;         +-|-+---+      +-|-+---+    |
;           v              v          |
;         +---+          +---+        |
;         | a |          | b |        |
;         +---+          +---+        |
;           +-------------------------+
;           v
;         +---+---+      +---+---+
;    y -->| * | *-+----->| * | / |
;         +-|-+---+      +-|-+---+
;           v              v
;         +---+          +---+
;         | c |          | d |
;         +---+          +---+
;

(defvar w (append! x y))

; (cdr x) at this point will be '(b c d) as we used the destructive append.

;           w
;           |
;           v
;         +---+---+      +---+---+
;    x -->| * | *-+----->| * | * +----+
;         +-|-+---+      +-|-+---+    |
;           v              v          |
;         +---+          +---+        |
;         | a |          | b |        |
;         +---+          +---+        |
;           +-------------------------+
;           v
;         +---+---+      +---+---+
;    y -->| * | *-+----->| * | / |
;         +-|-+---+      +-|-+---+
;           v              v
;         +---+          +---+
;         | c |          | d |
;         +---+          +---+
;


; Exercise 3.13

;           +-----------------------------------------+
;           |                                         |
;           v                                         |
;         +---+---+      +---+---+      +---+---+     |
;   z --> | * | *-+----->| * | *-+----->| * | *-+-----+
;         +-|-+---+      +-|-+---+      +-|-+---+
;           v              v              v
;         +---+          +---+          +---+
;         | a |          | b |          | c |
;         +---+          +---+          +---+
;

; The program crashes - infinite loop.


; Exercise 3.14

(defun mystery (x)
  (labels ((iter (x y)
             (if (null x)
                 y
                 (let ((temp (cdr x)))
                   (set-cdr! x y)
                   (iter temp x)))))
    (iter x '())))


; Reverses a list in place. You need to reset the variable, however, as it
; will still point to what was the first element in the list and is now the last.

; Before:

;         +---+---+      +---+---+      +---+---+      +---+---+
;   v --> | * | *-+----->| * | *-+----->| * | *-+----->| * | / |
;         +-|-+---+      +-|-+---+      +-|-+---+      +-|-+---+
;           v              v              v              v
;         +---+          +---+          +---+          +---+
;         | a |          | b |          | c |          | d |
;         +---+          +---+          +---+          +---+

; After:


;                                                        v
;
;                                                        |
;                                                        v
;         +---+---+      +---+---+      +---+---+      +---+---+
;   w --> | * | *-+----->| * | *-+----->| * | *-+----->| * | / |
;         +-|-+---+      +-|-+---+      +-|-+---+      +-|-+---+
;           v              v              v              v
;         +---+          +---+          +---+          +---+
;         | d |          | c |          | b |          | a |
;         +---+          +---+          +---+          +---+



; Exercise 3.15

(defun set-to-wow! (x)
  (set-car! (car x) 'wow)
  x)

;; GRRR MORE BOX POINTER DIAGRAMS!

;               +---+---+
;         z1 -->| * | * |
;               +-|-+-|-+
;                 V   V
;               +---+---+     +---+---+
;          x -->| * | *-+---->| * | / |
;               +-|-+---+     +-|-+---+
;                 V             V
;               +---+         +---+
;               | a |         | b |
;               +---+         +---+

; becomes ...

;               +---+---+
;         z1 -->| * | * |
;               +-|-+-|-+
;                 V   V
;               +---+---+     +---+---+
;          x -->| * | *-+---->| * | / |
;               +-|-+---+     +-|-+---+
;                 V             V
;               +-----+       +---+
;               | wow |       | b |
;               +-----+       +---+



;               +---+---+     +---+---+     +---+---+
;         z2 -->| * | *-+---->| * | *-+---->| * | / |
;               +-|-+---+     +-|-+---+     +-|-+---+
;                 |             V             V
;                 |           +---+         +---+
;                 |           | a |         | b |
;                 |           +---+         +---+
;                 |             ^             ^
;                 |             |             |
;                 |           +-|-+---+     +-|-+---+
;                 +---------->| * | *-+---->| * | / |
;                             +---+---+     +---+---+

; becomes ...

;               +---+---+     +---+---+     +---+---+
;         z2 -->| * | *-+---->| * | *-+---->| * | / |
;               +-|-+---+     +-|-+---+     +-|-+---+
;                 |             V             V
;                 |           +---+         +---+
;                 |           | a |         | b |
;                 |           +---+         +---+
;                 |                           ^
;                 |                           |
;                 |           +---+---+     +-|-+---+
;                 +---------->| * | *-+---->| * | / |
;                             +-|-+---+     +---+---+
;                               |
;                               v
;                             +-----+
;                             | wow |
;                             +-----+


; Exercise 3.16

(defun count-pairs (x)
  (if (or (not (listp x))
          (null x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3:
;
;      +---+---+   +---+---+   +---+---+
;   -->|   | *-+-->|   | *-+-->|   | / |
;      +---+---+   +---+---+   +---+---+
;

(defparameter count-3 '(a b c))

; 4:
;
;      +---+---+   +---+---+   +---+---+
;   -->|   | *-+-->| * | *-+-->|   | / |
;      +---+---+   +-|-+---+   +---+---+
;                    |           ^
;                    |           |
;                    +-----------+
;

(defparameter count-4 '(a b c))
(setf (cadr count-4)
      (cddr count-4))

;
; 7:
;
;        +-----------+
;        |           |
;        |           v
;      +-|-+---+   +---+---+   +---+---+
;   -->| * | *-+-->| * | *-+-->|   | / |
;      +---+---+   +-|-+---+   +---+---+
;                    |           ^
;                    |           |
;                    +-----------+

(defparameter count-7 '(a b c))
(setf (cadr count-7)
      (cddr count-7))
(setf (car count-7)
      (cdr count-7))

;
; never:
;
;        +---------------------------+
;        v                           |
;      +---+---+   +---+---+   +---+-|-+
;   -->|   | *-+-->|   | *-+-->|   | * |
;      +---+---+   +---+---+   +---+---+

(defparameter count-infinity '(a b c))
(setf (cdr (last-pair count-infinity))
      count-infinity)

; Exercise 3.17

(defun make-set ()
  '())

(defun add-to-set (x set)
  (cons x set))

(defun element-of-set (x set)
  (not (null (find x set :test #'eq))))

; Let's make a functional version ...
(defun count-distinct-pairs (x)
  (labels ((iter (seq seen)
             (if (or (not (listp seq))
                     (null seq)
                     (element-of-set seq seen))
                 0
                 (let ((seen (add-to-set seq seen)))
                   (+ 1
                      (iter (car seq) seen)
                      (iter (cdr seq) seen))))))
    (iter x (make-set))))

; Exercise 3.18

(defun contains-cycle (seq)
  (labels ((iter (seq seen)
             (cond ((null seq) nil)
                   ((element-of-set seq seen) t)
                   (t (iter (cdr seq) (add-to-set seq seen))))))
    (iter seq (make-set))))

; Exercise 3.19

(defun contains-cycle-2 (front)
  (labels ((iter (seq)
             (cond ((null seq) nil)
                   ((eq seq front) t)
                   (t (iter (cdr seq))))))
    (iter (cdr front))))

; Have since realized this doesn't work for all cases, as a cycle could occur
; without containing the first element of the list.

; You could do it like this,

(defun contains-cycle-3 (front)
  (labels ((position-of (x seq &optional (acc 0))
             (if (eq x seq)
                 acc
                 (position-of x (cdr seq) (1+ acc))))
           (iter (seq n)
             (cond ((null seq) nil)
                   ((/= (position-of seq front) n) t)
                   (t (iter (cdr seq) (1+ n))))))
    (iter front 0)))
           
; but although it only uses constant space, it's not very efficient.

; So there's probably something I'm missing.


; Exercise 3.20

;              +------------------------------------------------------------------+
;  global  --> | cons -+            cdr           set-car! -+                 x
;  env         | car   |                          set-cdr!  |                 z
;              +-------|--------^---------------------------|--------^------------+
;                      |        |                           |        |
;                      |        |                           |        |
;                      v        |                           v        |
;                  +---+---+    |                       +---+---+    |
;                  | 0 | 0 +----+                       | 0 | 0 +----+
;                  +-|-+---+                            +-|-+---+
;                    |                                    |
;                    v                                    v
;            parameters: x y                     parameters: z new-value
;            body: (define (set-x! v) ... )      body: ((z 'set-cdr!) new-value)
;                  (define (set-y! v) ... )
;                  (define (dispatch m) ... )
;                  dispatch
;                                                      (etc. not going to list every method
;                                                            definition)
;
;
;                                  global env
;  --------------------------------------------------------------------------------------
;
;
;  -----------------^---------------------^------------------------------------^---------
;                   |                     |                                    |      (cdr z)
;           +---------------+    +--------------------+              +---------+----------+
;(cons 1 2) | x: 1          |    | x: (E1's dispatch) |  (cons x x)  | z: (E2's dispatch) | <- E3
;           | y: 2          |    | y: (E1's dispatch) |              +--------------------+
;    E1 --> | set-x!        |    | set-x!             | <-- E2
;           | set-y!        |    | set-y!             |
;    +----->| dispatch      |    | dispatch           |
;    |      +----|-------^--+    +--------------------+
;    |           |       |  ^             ^
;    |           v       |  |             |        (z 'cdr)
;    |       +---+---+   |  |       +-----+------+
;    |       | 0 | 0-+---+  |       | m: 'cdr    |<-- E4
;    |       +-|-+---+      |       +------------+
;    |         v            |
;    |   parameters: m      |    +-------------+
;    |   body: ...          +----| m: 'set-car!|<-- E6
;    |                      |    +-------------+
;  +-------------+          |            (z 'set-car!)
;  | v: 17       |<--- E7   +-------+
;  +-------------+                  |
;     ((z 'set-car!) new-value)     |            (z 'car)
;                                   |    +---------+
;                                   +----| m: car  |<--- E9
;                                        +---------+
;
;
;  --------------------------------------------------------------------------------------
;
;
;  -----------^-----------------------------------^--------------------------------------
;             |                                   |
;             |  (set-car! (cdr z) 17)            | (car x)
;  +---------------------+               +--------------------+
;  | z: (E1's dispatch)  |               | z: (E1's dispatch) |<--- E8
;  | new-value: 17       | <- E5         +--------------------+
;  +---------------------+
;
