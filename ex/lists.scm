#lang racket

(require berkeley)

;; ex 2.17

;; Example:
;; (last-pair (list 23 72 12))
;; (12)
(define (last-pair l)
  (cond ((null? l) (error "last-pair must take a non-empty list"))
        ((= 1 (length l)) l)
        (else (last-pair (cdr l)))))

;; (last-pair '()) ; errors
;; (last-pair (list 12 121231 1))

;; ex 2.18
(define (reverse l)
  (if (null? l) l
  (append (reverse (cdr l)) (list (car l)))))


;; ex 1.20

(define (same-parity n . l)
  (cons n
        (filter (if (even? n) even? odd?) l)))


;; ex 2.22

(define (square-list-car items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-car (cdr items)))))

(define (square-list items)
  (map square items))


;; ex 2.23

(define (for-each proc items)
  (cond ((null? items) '())
        (else
         (proc (car items))
         (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

