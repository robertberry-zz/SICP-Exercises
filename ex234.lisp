; Answers for 2-3-4

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (x)
  (cadr x))

(defun weight-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun left-branch (tree)
  (car tree))

(defun right-branch (tree)
  (cadr tree))

(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(defun decode (bits tree)
  (labels ((choose-branch (bit branch)
             (cond ((= bit 0) (left-branch branch))
                   ((= bit 1) (right-branch branch))
                   (t (error "bad bit -- CHOOSE-BRANCH"))))
           (iter (bits current-branch)
             (if (null bits)
                 '()
                 (let ((next-branch
                        (choose-branch (car bits) current-branch)))
                   (if (leaf? next-branch)
                       (cons (symbol-leaf next-branch)
                             (iter (cdr bits) tree))
                       (iter (cdr bits) next-branch))))))
    (iter bits tree)))


(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (t (cons (car set)
                 (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


; Exercise 2.67

(defparameter +sample-tree+
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(defparameter +sample-message+
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; CL-USER> (decode +sample-message+ +sample-tree+)
;; (A D A B B C A)


; Exercise 2.68

(defun encode (message tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Todo, rewrite this to take advantage of ordered representation?
(defun set-contains (set x)
  (cond ((null set) nil)
        ((eq x (car set)) t)
        (t (set-contains (cdr set) x))))

(defun tree-contains (tree symbol)
  (set-contains (symbols tree) symbol))

(defun encode-symbol (symbol tree)
  (labels ((iter (subtree)
             (if (leaf? subtree)
                 (if (eq symbol (symbol-leaf subtree))
                     '()
                     (error "symbol not in tree -- ENCODE-SYMBOL"))
                 (let ((left (left-branch subtree))
                       (right (right-branch subtree)))
                   (cond ((tree-contains left symbol) (cons 0 (iter left)))
                         ((tree-contains right symbol) (cons 1 (iter right)))
                         (t (error "symbol not in tree -- ENCODE-SYMBOL")))))))
    (iter tree)))

;; CL-USER> (encode '(A D A B B C A) +sample-tree+)
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)


; Exercise 2.69

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (leaf-set)
  (cond ((null leaf-set) (error "Empty leaf set -- SUCCESSIVE-MERGE"))
        ((= (length leaf-set) 1) (car leaf-set))
        (t (let ((first (car leaf-set))
                 (second (cadr leaf-set))
                 (rest (cddr leaf-set)))
             (successive-merge (adjoin-set (make-code-tree first second) rest))))))


; Exercise 2.70

(defparameter +rock-pairs+
  '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(defparameter +rock-message+
  "       Get a job

          Sha na na na na na na na na

          Get a job

          Sha na na na na na na na na

          Wah yip yip yip yip yip yip yip yip yip

          Sha boom
")

(defun words (string)
  "Pretty inefficient function to extract a list of words from a string!"
  (labels ((space? (char)
             (or (char= char #\Space)
                 (char= char #\Newline))))
    (reverse (cdr (reduce (lambda (words char)
                            (if (space? char)
                                (if (string= (car words) "")
                                    words
                                    (cons "" words))
                                (cons (concatenate 'string
                                                   (car words)
                                                   (string char))
                                      (cdr words))))
                          string :initial-value '(""))))))

(defun string->symbols (string)
  (map 'list #'intern (words string)))

(defparameter +rock-tree+
  (generate-huffman-tree +rock-pairs+))

(defparameter +encoded-rock+
  (encode (string->symbols (string-upcase +rock-message+)) +rock-tree+))

;; CL-USER> +encoded-rock+
;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1
;;  0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1
;;  0 1 1 0 1 1)

;; CL-USER> (length +encoded-rock+)
;; 84

; (so, 84 bits is required to encode the track)

;; For a fixed length encoding, we need to be able to distinguish between 8
;; different words, which requires log 8 (to the base 2), which is 3 bits per
;; word.

;; CL-USER> (length (words +rock-message+))
;; 36

;; 36 * 3 = 108 bits for the message. So the huffman-encoded message is better
;; compressed.


; Exercise 2.71

;; n = 5

;                  /  \
;                16    (15)
;                     /    \
;                    8      (7)
;                         /     \
;                        4      (3)
;                              /    \
;                              1     2

;; n = 10

;
;    /  \
; 1024   (1023)
;        /  \
;      256   (255)
;            /   \
;          128   (127)
;                /   \
;               64    (63)
;                     /   \
;                   32     (31)
;                          /  \
;                        16    (15)
;                             /    \
;                            8      (7)
;                                 /     \
;                                4      (3)
;                                      /    \
;                                      1     2

; The most frequent symbol is always at one of the first branches, so 1 bit.
; The least frequent symbol is always one of the last two branches, so n - 1 bits.


; Exercise 2.72

; Assuming the tree is balanced, it takes O(log n) steps to traverse. At each
; level of recursion, the algorithm must check whether the set in the left and
; right branch contains the symbol, which takes O(n) steps in the worst case
; scenario. n should roughly diminish by half with each level of recursion.

; TODO figure out what this means as an answer ...

; In the worst case scenario (the highly unbalanced tree of 2.71), the order
; of growth is O(1) for the most frequent symbol, as it will always be in one
; of the first nodes of the tree ... (Or will it be O(n) as it must still
; traverse the set?)

; The least frequent symbol will take roughly n levels of recursion. At each
; level the sets to check are diminished only by 1 item, so it will be roughly
; O(n!) amount of work.

