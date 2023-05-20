#lang racket

(require berkeley)

;; ex 2.24

(list 1 (list 2 (list 3 4)))


;; ex 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; '(1 2 4 4 5 6)
(cons x y)   ; '((1 2 3) 4 5 6)
(list x y)   ; '((1 2 3) (4 5 6)


;; ex 2.29

;; each arg is a branch
(define (make-mobile left right)
  (list left right))

;; structure is a number (weight) or another mobile
(define (make-branch length structure)
  (list length structure))

;; a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;; b.

(define (branch-structure branch)
  (cadr branch))

(define (branch-length branch)
  (car branch))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (cond ((null? branch) 0)
          ((null? struct) 0)
          ((number? struct) struct)
          (else (total-weight struct)))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; test
(total-weight
 (make-mobile (make-branch 1 20)
              (make-branch 1 (make-mobile (make-branch 1 22)
                                          (make-branch 1 13)))))

;; c.

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))


(define (sub-balanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

;; test: sub-balanced?

;; #t
(sub-balanced?
 (make-mobile (make-branch 1 1)
              (make-branch 1 1)))

;; #f
(sub-balanced?
 (make-mobile (make-branch 1 1)
              (make-branch 1 2)))

;; #t
(sub-balanced?
 (make-mobile (make-branch 2 5)
              (make-branch 10 1)))


(define (mobile? structure)
  (not (number? structure)))

;; test: mobile?
(mobile? (make-mobile (make-branch 1 1)
                      (make-branch 1 1)))

(define (left-submobile mobile)
  (let ((struct (branch-structure (left-branch mobile))))
     (if (mobile? struct)
         struct
         '())))

;; test: left-submobile
(left-submobile
 (make-mobile (make-branch 1 1)
              (make-branch 1 1)))

(left-submobile
 (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 3)))
              (make-branch 1 1)))


(define (right-submobile mobile)
  (let ((struct (branch-structure (right-branch mobile))))
     (if (mobile? struct)
         struct
         '())))

(define (balanced? mobile)
  (if (null? mobile)
      #t
      (and (sub-balanced? mobile)
           (balanced? (left-submobile mobile))
           (balanced? (right-submobile mobile)))))


;; test: balanced?

;; #t
(balanced?
 (make-mobile (make-branch 1 1)
              (make-branch 1 1)))

;; #f
(balanced?
 (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 3)))
              (make-branch 1 1)))

;; #t
(balanced?
 (make-mobile (make-branch 1 (make-mobile (make-branch 2 2) (make-branch 1 4)))
              (make-branch 1 6)))



;; ex 2.30

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree2 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree2 (car tree))
                    (square-tree2 (cdr tree))))))

;; test square-tree
(square-tree
 (list 1
       (list 2 (list 3 4) 5
             (list 6 7))))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5
             (list 6 7))))


;; ex 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree3 tree) (tree-map square tree))

(square-tree3
 (list 1
       (list 2 (list 3 4) 5
             (list 6 7))))


;; ex 2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
       (append rest
               (map (lambda (x) (cons (car s) x)) rest)))))

;; test: subsets
(subsets '(1 2 3))


;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)

;; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product
 '(2 5)
 '(3 1))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector
 '((1 2 1)
   (0 1 0)
   (2 3 4))
 '(2 6 1))

(define (transpose m)
  (accumulate-n cons '() m))

(transpose
 '((1 2)
   (3 4)
   (5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))
         m)))

(matrix-*-matrix
 '((1 2 1)
   (0 1 0)
   (2 3 4))
 '((2 1)
   (6 1)
   (1 1)))


;; ex 2.38

; Fold right

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate /  1 (list 1 2 3))
(fold-left /  1 (list 1 2 3))
(accumulate list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; commutative
