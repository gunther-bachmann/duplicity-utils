#! /usr/bin/env racket
#lang racket


;; see https://stackoverflow.com/questions/18877881/how-to-use-typed-racket-in-scribble-lp
;; https://docs.racket-lang.org/scribble/srcdoc.html
(module tp racket
  (require scribble/srcdoc
           (for-doc racket/base scribble/manual))
  (module def typed/racket

    (provide gbx)
    (: gbx (-> Integer String String))
    (define (gbx i s)
      s)

    (provide fib)
    (: fib (-> Integer Integer))
    (define (fib n)
      (cond [(= n 0)  0]
            [(<= n 2) 1]
            [#t       (+ (fib (sub1 n)) (fib (- n 2)))])))

  (require 'def)
  (gbx 1 "s")
  (provide
   (proc-doc/names fib
                   (-> integer? integer?)
                   (n)
                   ("Computes the")))

  (fib 10)

)
