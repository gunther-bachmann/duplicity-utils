#! /usr/bin/env racket
#lang scribble/lp2
@(require scribble/manual)

First steps in literate programming.
To generate scribble execute 'raco scribble literate-p.rkt'

@chunk[<xmain>
       (module xmain typed/racket
         (provide x)

         (: x Integer)
         (define x 2)

         (printf "xmain - executed when called from command line\n")

         (module+ test
           (require typed/rackunit)
           (check-equal? 1 1)))]

next chunk

@chunk[<ymain>
       (module ymain typed/racket ;; define submodules to use own language (e.g. typed/racket)
         (require (submod ".." xmain)) ;; import all from xmain
         (provide y)

         (: y Integer)
         (define y 3)

         (printf "ymain - executed when called from command line\n")

         ;; have a failing test (to see that it is actually executed
         (module+ test
           (require typed/rackunit)
           (check-equal? x y)))]

@chunk[<*>
       <xmain> ;; make sure to insert all chunks in the main (which is this)
       <ymain>

       ;; execute something dependend on the previously defined chunks
       (module main typed/racket
         (require (submod ".." ymain))
         (printf "main - executed when called from command line (~a)\n" y))

       ;; now include all test modules of the submodules
       ;; so that raco test ... works and C-c C-t from w/i emacs
       (module test typed/racket
         (require (submod ".." xmain test))
         (require (submod ".." ymain test)))]
