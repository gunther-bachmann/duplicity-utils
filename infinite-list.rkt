#lang lazy

(require racket/set)
(require lazy/force)

;; An infinite list:
(define fibs
  (list* 0 1 (map + fibs (cdr fibs))))

;; Print the first 15 Fibonacci numbers:
(println (!! (take 15 fibs)))
;; Print the 10000th fib number
(println (!! (list-ref fibs 10000)))
