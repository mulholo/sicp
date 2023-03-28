#lang racket

(require berkeley)


;;; Data types
;;;
;;; Card     ; A word representing a playing card.
;;;            Consists of a value and house e.g. 'jh = jack of hearts
;;;
;;; Hand     ; A sentence of card words
;;;
;;; Strategy ; A procedure that takes a Hand and the next face up Card and returns a boolean indicateing whether to hit


(define (ace? x) (equal? 'A x))

;; Q1

;; Get the card rank without suit
;;
;; Examples:
;;
;; (rank '1H)   ; 1
;; (rank '10D)  ; 10
;; (rank 'JS)   ; J
(define (rank card)
  (if (and (equal? 1 (first card))       ; Is first char 1?
           (equal? 0 (first (bf card)))) ; Is second char 0? i.e. is this 10?
      10
      (first card)))

;; Return the suit of a card
;;
;; Examples:
;;
;; (suit '1H) ; h
(define (suit card)
  (last card))


;; Given a hand, calculate its total number of points.
;; If the hand contains aces, return the largest possible
;; total that is less than or equal to 21.
;;
;; Examples:
;;
;; (best-total '(AD 8S))    ; 19
;; (best-total '(AD 8S 5H)) ; 14
;; (best-total '(AD AS 9H)) ; 21
(define (best-total hand)
  (define (simple-score rank)
    (cond ((equal? 'K rank) 10)
          ((equal? 'Q rank) 10)
          ((equal? 'J rank) 10)
          (else rank)))
  
  (define (final-score total num-aces)
    (cond ((equal? 0 num-aces)           ; If no aces, simply return the total
           total)
          ((> 11 (+ total num-aces))     ; If we don't blow past 21, set one ace to 11, the rest to 1
           (+ total (dec num-aces) 11))  ; (we know we'll have max 1 ace as (11*2)>21)
          (else
            (+ total num-aces))))        ; Otherwise, set all aces to 1
  
  (define (go num-aces total remaining)
    (cond ((equal? '() remaining)
           (final-score total num-aces))
          ((ace? (first remaining))
           (go (inc num-aces)
               total
               (bf remaining)))
          (else
           (go num-aces
               (+ total (simple-score (first remaining)))
               (bf remaining)))))
            
  (go 0 0 (every rank hand)))

  
;; Tests
; (best-total '())         ; 0
; (best-total '(7H))       ; 7
; (best-total '(5H 6H 9D)) ; 20
; (best-total '(AD 8S))    ; 19
; (best-total '(AD 8S 5H)) ; 14
; (best-total '(AD AS 9H)) ; 21
; (best-total '(9H 9S 8D)) ; 26

;; Plays a game using the given strategy and a randomly shuffled deck
;; Returns 1 for customer win, 0 for draw, or -1 for customer loss
(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

;; Q2

(define (stop-at-17 customer-hand-so-far dealer-up-card)
  (> 17 (best-total customer-hand-so-far)))
; (trace stop-at-17)

;; Q3

;; Plays n games with a given strategy
(define (play-n strategy n)
  (define (iter total num-games)
    (if (= n num-games)
        total
        (iter (+ total (twenty-one strategy)) (inc num-games))))
  (iter 0 0))

;; Q4

;; Hits IFF dealer has ace, 7, 8, 9, 10, picture & customer has < 17
;;          OR dealer has 2, 3, 4, 5, 6 and customer < 12
(define (dealer-sensitive customer-hand-so-far dealer-up-card)
  (let ((customer-total (best-total customer-hand-so-far))
        (dealer-rank (rank dealer-up-card)))
    (or (and (member? dealer-rank '(A 7 8 9 10 J Q K))
              (< customer-total 17))
         (and (member? dealer-rank '(2 3 4 5 6))
              (< customer-total 12)))))

;; Q5

;; Build a strategy that stops when at score n
(define (stop-at n)
  (lambda (customer-hand dealer-card)
    (> n (best-total customer-hand))))


;; Q6

;; Returns true if the hand contains the target suit
(define (has-suit? target-suit hand)
  (member? target-suit (every suit hand)))


;; Strategy to play until 19 if you have a heart, 17 otherwise
(define (valentine customer-hand dealer-card)
  ((stop-at (if (has-suit? 'H customer-hand) 19 17))
   customer-hand dealer-card))


;; Q7

(define (suit-strategy target-suit if-suit if-not-suit)
  (lambda (customer-hand dealer-card)
    ((if (has-suit? target-suit customer-hand)
         if-suit
         if-not-suit)
     customer-hand dealer-card)))

(define valentine-2 (suit-strategy 'H (stop-at 19) (stop-at 17)))

;; Q8

;; Takes 3 strategies and produces a new strategy which returns
;; true iff 2 or more of the strategies vote to hit
(define (majority a b c)
  (lambda (customer-hand dealer-card)
    (let ((res-a (a customer-hand dealer-card))  ; Calculate the results of the 3 strategies
          (res-b (b customer-hand dealer-card))
          (res-c (c customer-hand dealer-card)))
      (or (and (res-a res-b res-c))              ; All 3 strategies agree to hit...
          (and (res-a res-b))                    ; ...or any 2 others do
          (and (res-a res-c))
          (and (res-b res-c))))))

;; Q9

(define (reckless strategy)
  (lambda (customer-hand dealer-card)
    (or (strategy customer-hand dealer-card)
        (and (< 2 (count customer-hand))
             (strategy (butlast customer-hand) dealer-card)))))


(define recklessly (reckless (stop-at 16)))

(trace recklessly)

(play-n recklessly 10)
