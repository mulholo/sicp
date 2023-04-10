#lang racket

(require berkeley)

;; Constructors

(define (make-interval low up) (cons low up))

;; Selectors

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))


;; Lib

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; ex 2.10
(define (spans-zero? interval)
  (and (>= 0 (lower-bound interval))
       (<= 0 (upper-bound interval))))

(define (div-interval x y)
  (if (spans-zero? y) (error "Cannot divide by an interval that spans zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))


;; ex 2.12

;; Constructs an interval with an absolute tolernace
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Constructs an interval with percentage tolernace
(define (make-center-percent c pct)
  (let ((w (* c pct)))
    (make-center-width c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


