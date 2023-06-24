#lang racket

(require berkeley)

; divisions
; - marketing
; - sales
; - engineering

; a) sales

(define (install-sales)

  ;; internal lib
  (define (make-record name salary)
    (list name salary))

  (define (get-name record)
    (car record))

  (define (get-salary record)
    (cadr record))

  (define (add-record record file)
    (cons record file))

  (define (get-record file name)
    (let ((r (car file)))
      (if (equal? name (get-name r))
          r
          (get-record (cdr file) name))))

  ;; external api
  (put 'make-record 'sales make-record))


; find-employee-record ; search all devisions files for the record of a given employee and return it
; (name files) ; list of files

