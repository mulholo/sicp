- appender
-

```rkt

;; given a fn, apply it to str twice
(define (str-manipulater str fn) (fn (fn str)))

```
