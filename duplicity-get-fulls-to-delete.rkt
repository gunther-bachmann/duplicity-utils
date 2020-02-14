#! /usr/bin/env racket
#lang typed/racket

;; problem is that no racket packages can be installed into the racket-minimal installation used by shell-nix
;; #! /usr/bin/env nix-shell
;; #! nix-shell -i racket -p racket-minimal

(require/typed racket
  [stream->list (-> (Sequenceof Byte) (Listof Byte))])

;; raco pkg install gregor
(require/typed (prefix-in gg: gregor)
  [#:opaque Date gg:date?]
  [gg:parse-date (-> String String Date)]
  [gg:date=? (-> Date Date Boolean)]
  [gg:date (->* (Integer) (Integer Integer) Date)]
  [gg:now (-> Date)]
  )

(require/typed (prefix-in gg: gregor/period)
  [#:opaque Period gg:period?]
  [gg:period-between (-> Date Date (Listof Symbol) Period)]
  [gg:period-ref (-> Period Symbol Integer)])

(require typed/racket)

(define-type AgePathPair (List Integer Path))

(: full-backup-ls-pattern String)
(define full-backup-ls-pattern "duplicity-full-signatures\\..*\\.sigtar\\.gpg$")

(: process-config-line (->  String (HashTable Symbol String) (HashTable Symbol String)))
(define (process-config-line line configuration)
  (cond [(string-prefix? line "#")
           configuration]
        [(string-prefix? line "backup-folder: ")
         (hash-set configuration 'backup-folder (regexp-replace "^backup-folder: (.*)" line "\\1"))]
        [#t configuration]))

(: empty-config-hash (HashTable Symbol String))
(define empty-config-hash (hash))

(: read-configuration (-> String (HashTable Symbol String)))
(define (read-configuration file-name)
  (foldr (lambda ([arg : String] [acc : (HashTable Symbol String)]) (process-config-line arg acc)) empty-config-hash (file->lines file-name)))

(module+ test
  (require typed/rackunit)
  (: valid-path-20200101 Path)
  (define valid-path-20200101
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200101T182223Z.sigtar.gpg"))
  (: valid-path-20200201 Path)
  (define valid-path-20200201
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (: valid-path-20200203 Path)
  (define valid-path-20200203
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200203T112223Z.sigtar.gpg"))
  (: valid-path-20200514 Path)
  (define valid-path-20200514
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200514T082223Z.sigtar.gpg"))
  (: valid-path-20200502 Path)
  (define valid-path-20200502
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200502T092223Z.sigtar.gpg"))
  (: invalid-path Path)
  (define invalid-path
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signures.20200201T172223Z.sigtar.gpg")))

(: matched-backup-file (-> Path (U (Pairof String (Listof (U False String))) False)))
(define (matched-backup-file path)
  (regexp-match full-backup-ls-pattern (path->string path)))

(module+ test
  (check-equal? (matched-backup-file valid-path-20200201)
                '("duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (check-false (matched-backup-file invalid-path)))

(: backup-date (-> Path Date))
(define (backup-date path)
  (gg:parse-date (regexp-replace ".*signatures\\.(.*)\\.sigtar.gpg" (path->string path) "\\1") "yyyyMMdd'T'HHmmssX"))

(module+ test
  (check gg:date=?
         (backup-date valid-path-20200201)
         (gg:date 2020 02 01))
  (check-exn exn:fail?
             (lambda () (backup-date invalid-path))))

(: backup-age-in-months (->* (Path) (Date) Integer))
(define (backup-age-in-months path [reference (gg:now)])
  (gg:period-ref (gg:period-between (backup-date path) reference '(months)) 'months))

(module+ test
  (check-equal? (backup-age-in-months valid-path-20200201 (gg:date 2020 07 01))
                5)
  (check-equal? (backup-age-in-months valid-path-20200203 (gg:date 2020 07 01))
                4)
  (check-equal? (backup-age-in-months valid-path-20200514 (gg:date 2020 07 01))
                1)
  (check-equal? (backup-age-in-months valid-path-20200502 (gg:date 2020 07 01))
                1))

(: pair-with-age (->* ((Listof Path)) (Date) (Listof AgePathPair)))
(define (pair-with-age paths [reference-date (gg:now)])
  (map (lambda ([path : Path]) `(,(backup-age-in-months path reference-date) ,path)) paths))

(module+ test
  (check-equal? (pair-with-age (list valid-path-20200201 valid-path-20200203) (gg:date 2020 07 01))
                `((5 ,valid-path-20200201) (4 ,valid-path-20200203))))

(: smaller-int (-> Integer Integer Boolean))
(define (smaller-int left right)
  (< left right))

(: age-path-pair-key (-> AgePathPair Integer))
(define (age-path-pair-key age-path-pair)
  (car age-path-pair))

(module UNTYPED racket/base

  (define (sort-by-age age-path-pairs)
    (sort age-path-pairs < #:key (lambda (pair) (car pair)))) ;; put into untyped region since type checker cannot work with polymorphic key-word parameter (racket 7.5)
    (provide sort-by-age))

(require/typed 'UNTYPED
  [sort-by-age ((Listof AgePathPair) -> (Listof AgePathPair)) ])

(module+ test
    (check-equal? (sort-by-age `((5 ,valid-path-20200201) (4 ,valid-path-20200203)))
                `((4 ,valid-path-20200203) (5 ,valid-path-20200201) )))

(: --fill-gaps (-> (Listof AgePathPair) Integer (Listof AgePathPair) (Listof AgePathPair)))
(define (--fill-gaps rest-sorted-age-path-pairs n result)
  (cond [(empty? rest-sorted-age-path-pairs)
         result]
        [(and (= 1 (length rest-sorted-age-path-pairs))
              (> n (first (first rest-sorted-age-path-pairs))))
         result]
        [(or (= 1 (length rest-sorted-age-path-pairs))
             (< n (first (second rest-sorted-age-path-pairs))))
         (--fill-gaps rest-sorted-age-path-pairs
                     (add1 n)
                     (cons `(,n ,(second (first rest-sorted-age-path-pairs))) result))]
        [(>= n  (first (second rest-sorted-age-path-pairs)))
         (--fill-gaps (cdr rest-sorted-age-path-pairs)
                     (add1 n)
                     (cons `(,n ,(second (second rest-sorted-age-path-pairs))) result))]
        [#t result]))

(module+ test
  (define a (string->path "a"))
  (define b (string->path "b"))
  (define c (string->path "c"))
  (check-equal? (--fill-gaps `((2 ,a)(3 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,b) (3 ,b) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((0 ,a)(2 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,b) (3 ,b) (2 ,b) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((1 ,a)(5 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,a) (3 ,a) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((2 ,a)(5 ,b)) 0 '())
                `( (5 ,b) (4 ,a) (3 ,a) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((2 ,a)) 0 '())
                `(  (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps '() 0 '())
                '()))

(: fill-gaps (-> (Listof AgePathPair) (Listof AgePathPair)))
(define (fill-gaps sorted-age-path-pairs)
  (reverse (--fill-gaps sorted-age-path-pairs 0 '())))

(: fib (-> Integer Integer))
(define (fib n)
  (cond [(= n 0)  0]
        [(<= n 2) 1]
        [#t       (+ (fib (sub1 n)) (fib (- n 2)))]))

(: fib-backup-ages-to-keep (Setof Integer))
(define fib-backup-ages-to-keep (list->set (map fib (stream->list (in-range 0 15)))))

(module+ test
  (check-true (set-member? fib-backup-ages-to-keep (fib 10)))
  (check-true (set-member? fib-backup-ages-to-keep 0))
  (check-false (set-member? fib-backup-ages-to-keep (- (fib 10) 1))))

(: keep-backup-since-age-is-fib? (-> AgePathPair Boolean))
(define (keep-backup-since-age-is-fib? age-backup-pair)
  (set-member? fib-backup-ages-to-keep (first age-backup-pair)))

(: age-path-pairs->paths (-> (Listof AgePathPair) (Setof Path)))
(define (age-path-pairs->paths age-path-pairs)
  (list->set (map (lambda ([pair : AgePathPair]) (second pair)) age-path-pairs)))

(module+ test
  (check-equal? (age-path-pairs->paths `((0 ,a) (1 ,a) (2 ,b) (3 ,b) (4 ,c)))
                (list->set `(,a ,b ,c))))

(module+ test
  (define d (string->path "d"))
  (define e (string->path "e"))
  (define f (string->path "f"))
  (define g (string->path "g"))
  (define h (string->path "h"))
  (define i (string->path "i"))
  (define j (string->path "j"))
  (define k (string->path "k"))
  (define l (string->path "l"))
  (define m (string->path "m"))
  (define n (string->path "n"))
  (define o (string->path "o"))
  (define p (string->path "p"))
  (define q (string->path "q"))
  (define r (string->path "r"))
  (define s (string->path "s"))
  (define t (string->path "t"))
  (define u (string->path "u"))
  (define v (string->path "v"))
  (define w (string->path "w"))
  (define x (string->path "x"))
  (define y (string->path "y"))
  (define z (string->path "z"))
  (check-equal? (kept-paths `(,a ,b ,c ,d ,e ,f ,g)
                            `((0 ,a) (1 ,b) (2 ,c) (3 ,d) (4 ,e) (5 ,f) (6 ,g)))
                (list->set `(,a ,b ,c ,d ,f ,g))) ;; dropped e
  (check-equal? (kept-paths `(,a ,b ,c ,d ,f ,g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g)))
                (list->set `(,a ,b ,c ,d ,e ,g)))
  (check-equal? (kept-paths `(,a ,b ,c ,d ,e ,g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g) (13 ,g) (14 ,g) (15 ,h)))
                (list->set `(,a ,b ,c ,d ,e ,g ,h)))
  (check-equal? (kept-paths `(,a ,b ,c ,d ,e ,g ,h)
                            (fill-gaps `((2 ,a) (3 ,b) (4 ,c)  (7 ,d) (8 ,e)  (9 ,f)  (12 ,g) (15 ,h))))
                (list->set `(,a ,b ,c ,d ,e ,g ,h)))

  (check-equal? (kept-paths `(,l ,m ,n ,o ,p ,r ,u ,z)
                            (fill-gaps `((0 ,l) (1 ,m) (2 ,n) (3 ,o) (4 ,p) (6 ,r) (9 ,u) (14 ,z))))
                (list->set `(,l ,m ,n ,o ,p ,r ,u ,z))) ;; drop none
  (check-equal? (kept-paths `(,k ,l ,m ,n ,o ,p ,r ,u ,z)
                            (fill-gaps `((0 ,k) (1 ,l) (2 ,m) (3 ,n) (4 ,o) (5 ,p) (7 ,r) (10 ,u) (15 ,z))))
                (list->set `(,k ,l ,m ,n ,p ,r ,u ,z))) ;; drop o
  (check-equal? (kept-paths `(,j ,k ,l ,m ,n ,p ,r ,u ,z)
                            (fill-gaps `((0 ,j) (1 ,k) (2 ,l) (3 ,m) (4 ,n) (6 ,p) (8 ,r) (11 ,u) (16 ,z))))
                (list->set `(,j ,k ,l ,m ,n ,r ,u ,z))) ;; drop p
  (check-equal? (kept-paths `(,i ,j ,k ,l ,m ,n ,r ,u ,z)
                            (fill-gaps `((0 ,i) (1 ,j) (2 ,k) (3 ,l) (4 ,m) (5 ,n) (9 ,r) (12 ,u) (17 ,z))))
                (list->set `(,i ,j ,k ,l ,m ,n ,r ,u ,z))) ;; drop none
  (check-equal? (kept-paths `(,h ,i ,j ,k ,l ,m ,n ,r ,u ,z)
                            (fill-gaps `((0 ,h) (1 ,i) (2 ,j) (3 ,k) (4 ,l) (5 ,m) (6 ,n) (10 ,r) (13 ,u) (18 ,z))))
                (list->set `(,h ,i ,j ,k ,m ,n ,r ,u ,z))) ;; drop l
  (check-equal? (kept-paths `(,g ,h ,i ,j ,k ,m ,n ,r ,u ,z)
                            (fill-gaps `((0 ,g) (1 ,h) (2 ,i) (3 ,j) (4 ,k) (6 ,m) (7 ,n) (11 ,r) (14 ,u) (19 ,z))))
                (list->set `(,g ,h ,i ,j ,k ,m ,n ,r ,z))) ;; drop u
  (check-equal? (kept-paths `(,f ,g ,h ,i ,j ,k ,m ,n ,r ,z)
                            (fill-gaps `((0 ,f) (1 ,g) (2 ,h) (3 ,i) (4 ,j) (5 ,k) (7 ,m) (8 ,n) (12 ,r)(20 ,z))))
                (list->set `(,f ,g ,h ,i ,j ,k ,m ,n ,r ,z))) ;; drop none
  (check-equal? (kept-paths `(,e ,f ,g ,h ,i ,j ,k ,m ,n ,r ,z)
                            (fill-gaps `((0 ,e) (1 ,f) (2 ,g) (3 ,h) (4 ,i) (5 ,j) (6 ,k) (8 ,m) (9 ,n) (13 ,r)(21 ,z))))
                (list->set `(,e ,f ,g ,h ,j ,m ,r ,z)))
  ) ;; drop i, k, n

(: --keep-because-it-becomes-fib (-> (Listof AgePathPair) Integer (Listof Path) Integer (Listof Path)))
(define (--keep-because-it-becomes-fib sorted-age-path-pairs n kept-paths fib-distance)
  (cond [(= n 0) kept-paths]
        [#t (let* ([age-path-pair (list-ref sorted-age-path-pairs n)]
                   [age           (car age-path-pair)])
              (--keep-because-it-becomes-fib sorted-age-path-pairs
                                            (sub1 n)
                                            (if (set-member? fib-backup-ages-to-keep (+ age fib-distance))
                                                (cons (cadr age-path-pair) kept-paths)
                                                kept-paths)
                                            fib-distance))]))

(module+ test
  (check-equal? (--keep-because-it-becomes-fib `((1 ,a) (5 ,b) (7 ,c) (15 ,d)) 3 '() 6)
                `(,c ,d)))

(: next-fib-ge (-> Integer Integer))
(define (next-fib-ge age)
  (first (sort (filter (lambda ([fnum : Integer]) (>= fnum age)) (set->list fib-backup-ages-to-keep)) <)))

(: keep-because-it-becomes-fib (-> (Listof AgePathPair) (Setof Path)))
(define (keep-because-it-becomes-fib sorted-age-path-pairs)
  (let* ([oldest-age                (car (last sorted-age-path-pairs))]
         [min-fib-older-than-oldest (next-fib-ge oldest-age)])
    (list->set (--keep-because-it-becomes-fib sorted-age-path-pairs
                                            (sub1 (length sorted-age-path-pairs))
                                            '()
                                            (- min-fib-older-than-oldest oldest-age)))))

(module+ test
  (check-equal? (keep-because-it-becomes-fib `((1 ,a) (5 ,b) (7 ,c) (15 ,d)))
                (set c d)))

(: kept-paths (-> (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (kept-paths all-paths age-path-pairs)
  (let* ([filled-age-file-pairs       (fill-gaps age-path-pairs)]
         [fib-filtered-age-file-pairs (filter keep-backup-since-age-is-fib? filled-age-file-pairs)]
         [fib-kept-paths              (age-path-pairs->paths fib-filtered-age-file-pairs)]
         [oldest-path                 (set (cadr (last age-path-pairs)))]
         [four-youngest               (list->set (take all-paths 4))]
         [fib-some-day-kept           (keep-because-it-becomes-fib filled-age-file-pairs)]
         [all-kept                    (set-union fib-kept-paths oldest-path four-youngest fib-some-day-kept)])
    all-kept))

(module+ test
  (check-equal? (fib 0) 0)
  (check-equal? (fib 1) 1)
  (check-equal? (fib 2) 1)
  (check-equal? (fib 3) 2)
  (check-equal? (fib 10) 55))

(module+ test
  (check-equal? (fill-gaps `((1 ,valid-path-20200514) (4 ,valid-path-20200203) (6 ,valid-path-20200101)))
                `((0 ,valid-path-20200514)
                  (1 ,valid-path-20200514)
                  (2 ,valid-path-20200514)
                  (3 ,valid-path-20200514)
                  (4 ,valid-path-20200203)
                  (5 ,valid-path-20200203)
                  (6 ,valid-path-20200101))))

(define (check-backups)
  (let* ([config     (read-configuration "/home/pe/.duplicity/config")]
         [backup-dir (hash-ref config 'backup-folder)])
    (if (not (directory-exists? backup-dir))
        (printf "dir ~s does not exist\n" backup-dir)
        (begin
          (printf "dir ~s exists\n" backup-dir)
          (let* ([full-backup-files  (directory-list backup-dir)]
                 [full-sig-files     (filter matched-backup-file full-backup-files)]
                 [age-file-pairs     (sort-by-age (pair-with-age full-sig-files))]
                 [all-kept           (kept-paths full-sig-files age-file-pairs)]
                 [discard            (set-subtract (list->set full-sig-files) all-kept)])
            (for-each (lambda ([arg : AgePathPair]) (printf "age: ~s, path: ~s\n" (first arg) (path->string (second arg)))) age-file-pairs)
            (printf "keeping ~s\n" all-kept)
            (printf "discard ~s\n" discard))))))

(: discard-all-related-to (-> Path String (Listof Path)))
(define (discard-all-related-to sig-file backup-dir)
  (let* ([date (regexp-replace ".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1")]
         [full-backup-files  (directory-list backup-dir)]
         [related-files (filter (lambda ([path : Path]) (regexp-match (format ".*\\.~a\\..*" date) (path->string path))) full-backup-files)])
    related-files))

(check-backups)

;; ;; (printf "Given arguments: ~s\n"
;; ;;         (current-command-line-arguments))
