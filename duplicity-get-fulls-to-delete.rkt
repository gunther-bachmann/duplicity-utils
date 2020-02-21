#! /usr/bin/env racket
#lang typed/racket

;; problem is that no racket packages can be installed into the racket-minimal installation used by shell-nix
;; #! /usr/bin/env nix-shell
;; #! nix-shell -i racket -p racket-minimal

(require typed/racket)

(require/typed racket
  [stream->list (-> (Sequenceof Byte) (Listof Byte))])

;; raco pkg install gregor
(require/typed (prefix-in gg: gregor)
  [#:opaque gg:Date gg:date?]
  [#:opaque gg:Datetime gg:datetime?]
  [gg:parse-date (-> String String gg:Date)]
  [gg:date=? (-> (U gg:Date gg:Datetime) (U gg:Date gg:Datetime) Boolean)]
  [gg:date (->* (Integer) (Integer Integer) gg:Date)]
  [gg:now (-> gg:Datetime)]
  )

(define-type Date (U gg:Date gg:Datetime))

(require/typed (prefix-in gg: gregor/period)
  [#:opaque Period gg:period?]
  [gg:period-between (-> Date Date (Listof Symbol) Period)]
  [gg:period-ref (-> Period Symbol Integer)])

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

(: keep-backup-since-age-is-kept? (-> (Setof Integer) AgePathPair Boolean))
(define (keep-backup-since-age-is-kept? backup-ages-to-keep age-backup-pair)
  (set-member? backup-ages-to-keep (first age-backup-pair)))

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
                (list->set `(,e ,f ,g ,h ,j ,m ,r ,z)))  ;; drop i, k, n
  )

(: --keep-because-it-becomes-relevant (-> (Listof AgePathPair) Integer (Listof Path) Integer (Listof Path)))
(define (--keep-because-it-becomes-relevant sorted-age-path-pairs n kept-paths fib-distance)
  (cond [(= n 0) kept-paths]
        [#t (let* ([age-path-pair (list-ref sorted-age-path-pairs n)]
                   [age           (car age-path-pair)])
              (--keep-because-it-becomes-relevant sorted-age-path-pairs
                                                 (sub1 n)
                                                 (if (set-member? fib-backup-ages-to-keep (+ age fib-distance))
                                                     (cons (cadr age-path-pair) kept-paths)
                                                     kept-paths)
                                                 fib-distance))]))

(module+ test
  (check-equal? (--keep-because-it-becomes-relevant `((1 ,a) (5 ,b) (7 ,c) (15 ,d)) 3 '() 6)
                `(,c ,d)))

(: next-age-ge (-> Integer (Setof Integer) Integer))
;; get next age greater or equal within the list of backup ages to keep
(define (next-age-ge age backup-ages-to-keep)
  (first (sort (filter (lambda ([fnum : Integer]) (>= fnum age)) (set->list backup-ages-to-keep)) <)))

(module+ test
  (check-equal? (next-age-ge 3 (set 1 2 7 9)) 7)
  (check-equal? (next-age-ge 3 (set 1 2 4 9)) 4)
  (check-equal? (next-age-ge 3 (set 1 3 9)) 3)
  (check-exn #rx".*contract violation.*" (lambda () (next-age-ge 10 (set 1 3 9)))))

(: keep-first-n (-> Integer (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (keep-first-n n all-paths age-path-pairs)
  (list->set (take all-paths (min (length all-paths) n))))

(: keep-oldest (-> (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (keep-oldest all-paths age-path-pairs)
  (set (cadr (last (sort-by-age age-path-pairs)))))

(: keep-by-age-list (-> (Setof Integer) (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (keep-by-age-list backup-ages-to-keep all-paths age-path-pairs)
  (let ([filled-age-file-pairs (fill-gaps (sort-by-age age-path-pairs))])
    (age-path-pairs->paths (filter (curry keep-backup-since-age-is-kept? backup-ages-to-keep) filled-age-file-pairs))))

(: keep-because-it-becomes-relevant (-> (Setof Integer) (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (keep-because-it-becomes-relevant backup-ages-to-keep all-paths age-path-pairs)
  (let* ([sorted-age-path-pairs     (fill-gaps (sort-by-age age-path-pairs))]
         [oldest-age                (first (last sorted-age-path-pairs))]
         [min-fib-older-than-oldest (next-age-ge oldest-age backup-ages-to-keep)])
    (list->set (--keep-because-it-becomes-relevant sorted-age-path-pairs
                                                 (sub1 (length sorted-age-path-pairs))
                                                 '()
                                                 (- min-fib-older-than-oldest oldest-age)))))

(module+ test
  (check-equal? (keep-because-it-becomes-relevant fib-backup-ages-to-keep (list a b c d) `((1 ,a) (5 ,b) (7 ,c) (15 ,d)))
                (set a c d)))

(: backup-keep-functions (Listof (-> (Listof Path) (Listof AgePathPair) (Setof Path))))
(define backup-keep-functions
  (list (curry keep-first-n 4)
        keep-oldest
        (curry keep-by-age-list fib-backup-ages-to-keep)
        (curry keep-because-it-becomes-relevant fib-backup-ages-to-keep)))

(: --kept-paths (-> (Listof Path) (Listof AgePathPair) (Setof Path) (Listof (-> (Listof Path) (Listof AgePathPair) (Setof Path))) (Setof Path)))
(define (--kept-paths all-paths age-path-pairs path-set keep-functions)
  (cond [(empty? keep-functions)
         path-set]
        [#t
         (--kept-paths all-paths
                      age-path-pairs
                      (set-union path-set
                                 ((first keep-functions) all-paths age-path-pairs))
                      (rest keep-functions))]))

(module+ test
  (check-equal? (--kept-paths '() '() (set) '())
                (set))
  (check-equal? (--kept-paths `(,valid-path-20200101)
                             `((1 ,valid-path-20200101))
                             (set)
                             backup-keep-functions)
                (set valid-path-20200101))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502))
                             (set)
                             (list keep-oldest))
                (set valid-path-20200502))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502, valid-path-20200514)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502)
                               (5 ,valid-path-20200514))
                             (set)
                             (list (curry keep-first-n 4)))
                (set valid-path-20200101 valid-path-20200201 valid-path-20200203 valid-path-20200502))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502, valid-path-20200514)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502)
                               (5 ,valid-path-20200514))
                             (set)
                             (list (curry  keep-by-age-list fib-backup-ages-to-keep)))
                (set valid-path-20200101 valid-path-20200201 valid-path-20200203 valid-path-20200514))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502, valid-path-20200514)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502)
                               (5 ,valid-path-20200514))
                             (set)
                             (list (curry keep-because-it-becomes-relevant fib-backup-ages-to-keep)))
                (set valid-path-20200101 valid-path-20200201 valid-path-20200203 valid-path-20200514))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502, valid-path-20200514)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502)
                               (7 ,valid-path-20200514))
                             (set)
                             (list (curry keep-because-it-becomes-relevant fib-backup-ages-to-keep)))
                (set valid-path-20200101 valid-path-20200201 valid-path-20200502 valid-path-20200514)))

(: kept-paths (-> (Listof Path) (Listof AgePathPair) (Setof Path)))
(define (kept-paths all-paths age-path-pairs)
  (--kept-paths all-paths age-path-pairs (set) backup-keep-functions))

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
  (printf "locating configuration\n")
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

(: get-full-related-to (-> Path String (Listof Path) (Listof Path)))
(define (get-full-related-to sig-file backup-dir full-backup-files)
  (let* ([date               (regexp-replace ".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1")]
         [related-files      (filter (lambda ([path : Path]) (regexp-match (format ".*duplicity-full\\.~a\\..*" date) (path->string path))) full-backup-files)])
    related-files))

(module+ test
  (check-equal? (get-to-datestr-of-chain (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))
                "20191011T121221Z")
  (check-equal? (get-from-datestr-of-chain (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))
                "20191011T115918Z"))

(: get-to-datestr-of-chain (-> Path String))
(define (get-to-datestr-of-chain manifest-file)
  (regexp-replace ".*duplicity-inc\\..*\\.to\\.(.*)\\.manifest\\..*" (path->string manifest-file) "\\1"))

(: get-from-datestr-of-chain (-> Path String))
(define (get-from-datestr-of-chain manifest-file)
  (regexp-replace ".*duplicity-inc\\.(.*)\\.to\\..*\\.manifest\\..*" (path->string manifest-file) "\\1"))

(module+ test
  (check-equal? (get-increment-based-on
                 (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg")))
                `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg"))))

(: get-increment-based-on (-> Path (Listof Path) (Listof Path)))
(define (get-increment-based-on manifest-file full-backup-files)
  (let* ([from-date          (get-from-datestr-of-chain manifest-file)]
         [to-date            (get-to-datestr-of-chain manifest-file)]
         [related-files      (filter (lambda ([path : Path]) (regexp-match (format ".*duplicity-inc\\.~a\\.to\\.~a\\..*" from-date to-date) (path->string path))) full-backup-files)])
    related-files))

(module+ test
  (check-equal? (get-manifest-based-on
                 "20191011T115918Z"
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg")))
                (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))

(: get-manifest-based-on (-> String (Listof Path) (U Path Void)))
(define (get-manifest-based-on from-date-str full-backup-files)
  (let* ([related-files      (filter (lambda ([path : Path]) (regexp-match (format ".*duplicity-inc\\.~a\\.to\\..*\\.manifest\\..*" from-date-str) (path->string path))) full-backup-files)])
    (when (not (empty? related-files))
      (first related-files))))

(module+ test
  (check-equal? (--get-all-increment-manifests-of-chain
                 "20191011T115918Z"
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg"))
                 '())
                `(,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))))

(: --get-all-increment-manifests-of-chain (-> String (Listof Path) (Listof Path) (Listof Path)))
(define (--get-all-increment-manifests-of-chain from-date full-backup-files collected-manifest-files)
  (let ([manifest (get-manifest-based-on from-date full-backup-files)])
    (if (path? manifest)
        (begin
          (--get-all-increment-manifests-of-chain (get-to-datestr-of-chain manifest) full-backup-files (cons manifest collected-manifest-files)))
        collected-manifest-files)))

(module+ test
  (check-equal? (get-chains-related-to
                 (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20191011T115918Z.sigtar.gpg")
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg")))
                `((,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg"))
                  (,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")))))

(: get-chains-related-to (-> Path (Listof Path) (Listof (Listof Path))))
(define (get-chains-related-to sig-file full-backup-files)
  (let* ([date                (regexp-replace ".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1")]
         [all-chain-manifests (--get-all-increment-manifests-of-chain date full-backup-files '())])
    (map (lambda ([manifest : Path]) (get-increment-based-on manifest full-backup-files)) all-chain-manifests)))

;; (check-backups)

;; (printf "Given arguments: ~s\n"
;;         (current-command-line-arguments))
