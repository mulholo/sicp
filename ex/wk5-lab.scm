#lang racket

(require berkeley)

;; ex 2.25

;; get 7 from each

(define x '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr x)))))

(define y '((7)) )
(car (car y))

(define z '(1 (2 (3 (4 (5 (6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr z))))))

;; ex 2.53

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))


;; ex 2.55

;; The folowing prints 'quote
;; This is because evaluating ''abracadabr gives: (quote (quote abracadabra))
;; This is the list of 'quote and 'abracadabra, so carring it grabs the quoted 'quote
(car ''abracadabra)


;; ex 2.27

(define (reverse l)
  (if (null? l) l
  (append (reverse (cdr l)) (list (car l)))))

(define (list-of-lists? seq)
  (list? (car seq)))

;; args:
;; l - a list
;;
;; returns:
;; a list with all of l elements reversed, including sublists
;;
;; example:
;; (define x (list (list 1 2) (list 3 4))) ; -> '((1 2) (3 4))
;; (reverse x)                             ; -> '((3 4) (1 2))
;; (deep-reverse x)                        ; -> '((4 3) (2 1))
(define (deep-reverse seq)
  (if (list-of-lists? seq)
      (reverse (map deep-reverse seq))
      (reverse seq)))
        
; test:
; (trace deep-reverse)
(newline)
(define a (list (list 1 2) (list 3 4)))
(reverse a)
(deep-reverse a)
