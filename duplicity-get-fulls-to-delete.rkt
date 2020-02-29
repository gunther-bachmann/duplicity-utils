#! /usr/bin/env racket
#lang typed/racket #:with-refinements

;; find out which months/generation/age can be deleted from backup given the following:
;; - full backups are done monthly, incrementals sub monthly
;; - keep a number (4) of most recent full
;; - keep the oldest
;; - keep all with a months age matching a fibonacci number
;; - keep those that will (in time) have a months age matching a fibonacci number,
;;   when the oldest backup matches a fibonacci number

;; automatic installation of racket packages is problematic:
;; problem is that no racket packages can be installed into the racket-minimal installation used by shell-nix
;; #! /usr/bin/env nix-shell
;; #! nix-shell -i racket -p racket-minimal

;; import gregorian dates with typing
(require/typed (prefix-in gg: gregor)
  [#:opaque gg:Date gg:date?]
  [#:opaque gg:Datetime gg:datetime?]
  [gg:parse-date (String String -> gg:Date)]
  [gg:date=? ((U gg:Date gg:Datetime) (U gg:Date gg:Datetime) -> Boolean)]
  [gg:date ((Integer) (Integer Integer) . ->* . gg:Date)]
  [gg:now (-> gg:Datetime)])

;; import gregorian periods with typing
(require/typed (prefix-in gg: gregor/period)
  [#:opaque Period gg:period?]
  [gg:period-between (Date Date (Listof Symbol) -> Period)]
  [gg:period-ref (Period Symbol -> Integer)])

;; treat Date and Datetime uniformly herein
(define-type Date (U gg:Date gg:Datetime))
;; age (in months) and file path
(define-type AgePathPair (List Nonnegative-Integer Path))
;; configuration fetched from file
(define-type Configuration (Immutable-HashTable Symbol String))

(: full-backup-ls-pattern Regexp)
;; matches full backups by one signature file
(define full-backup-ls-pattern #rx"duplicity-full-signatures\\..*\\.sigtar\\.gpg$")

(: known-config-keys (HashTable String Boolean))
;; configuration known and processed when reading the configuration
(define known-config-keys (hash "backup-folder" #t))

(module+ test #| process config line |#
  (check-false (hash-has-key? (process-config-line "some: value" (hash)) 'some))
  (check-true (hash-has-key? (process-config-line "backup-folder: value" (hash)) 'backup-folder))
  (check-equal? (hash-ref (process-config-line "backup-folder: value" (hash)) 'backup-folder)
                "value")
  (check-equal? (hash-ref (process-config-line "backup-folder: /some/path/to/backup" (hash)) 'backup-folder)
                "/some/path/to/backup"))

(: process-config-line : String Configuration -> Configuration)
;; process one configuration line and return enriched configuration
(define (process-config-line line configuration)
  (define matched (regexp-match #rx"^([^:]*): (.*)" line))
  (if matched
      (let ([key (list-ref (cdr matched) 0)] ;; regexp could return this to be #f
            [value (list-ref (cdr matched) 1)]) ;; same ...
        (if (and (string? key) ;; to ensure that resulting type is correct
                 (string? value) ;; same ...
                 (hash-has-key? known-config-keys key))
            (hash-set configuration (string->symbol key) value)
            configuration))
      configuration))

(: empty-config-hash Configuration)
;; seed
(define empty-config-hash (hash))

(module+ test #| read configuration |#
  (check-equal? (read-configuration '("backup-folder: some/folder" "# some comment" "unknown-prefix: ignored"))
                (hash 'backup-folder "some/folder")))

(: read-configuration : (Listof String) -> Configuration)
;; duplicity configuration
(define (read-configuration file-lines)
  (foldl (lambda ([arg : String] [acc : Configuration]) (process-config-line arg acc))
         empty-config-hash
         file-lines))


(module+ test #| setup test data |#
  (require typed/rackunit)
  (define a (string->path "a"))
  (define b (string->path "b"))
  (define c (string->path "c"))
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
  (: valid-path-20200101 Path)
  (define valid-path-20200101
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200101T182223Z.sigtar.gpg"))
  (define valid-path-20200201
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (define valid-path-20200203
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200203T112223Z.sigtar.gpg"))
  (define valid-path-20200514
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200514T082223Z.sigtar.gpg"))
  (define valid-path-20200502
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200502T092223Z.sigtar.gpg"))
  (define valid-path-20200605
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200605T182223Z.sigtar.gpg"))
  (define valid-path-20200701
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20200701T182223Z.sigtar.gpg"))
  (define valid-path-20201004
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20201004T182223Z.sigtar.gpg"))
  (define invalid-path
    (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signures.20200201T172223Z.sigtar.gpg")))

(module+ test #| matched-backup-file |#
  (check-equal? (matched-backup-file valid-path-20200201)
                '("duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (check-false (matched-backup-file invalid-path)))

(: matched-backup-file : Path -> (U (Pairof String (Listof (U False String))) False))
;; either #f or the matched signatur file (without path) of a full backup
(define (matched-backup-file path)
  (regexp-match full-backup-ls-pattern (path->string path)))

(module+ test #| backup date |#
  (check gg:date=?
         (backup-date valid-path-20200201)
         (gg:date 2020 02 01))
  (check-exn exn:fail?
             (lambda () (backup-date invalid-path))))

(: backup-date : Path -> Date)
;; get that date of a full backup signature file
(define (backup-date path)
  (gg:parse-date (regexp-replace #rx".*signatures\\.(.*)\\.sigtar.gpg" (path->string path) "\\1") "yyyyMMdd'T'HHmmssX"))

(module+ test #| backup age in months |#
  (check-equal? (backup-age-in-months valid-path-20200201 (gg:date 2020 07 01))
                5)
  (check-equal? (backup-age-in-months valid-path-20200203 (gg:date 2020 07 01))
                4)
  (check-equal? (backup-age-in-months valid-path-20200514 (gg:date 2020 07 01))
                1)
  (check-equal? (backup-age-in-months valid-path-20200502 (gg:date 2020 07 01))
                1))

(: backup-age-in-months ((Path) (Date) . ->* . Nonnegative-Integer))
;; age in months used as backup generation
(define (backup-age-in-months path [reference (gg:now)])
  (define result (gg:period-ref (gg:period-between (backup-date path) reference '(months)) 'months))
  (cond [(>= result 0) result]
        [else (error "backups cannot date in the future" path)]))


(module+ test #| pair with age |#
  (check-equal? (pair-with-age (list valid-path-20200201 valid-path-20200203) (gg:date 2020 07 01))
                `((5 ,valid-path-20200201) (4 ,valid-path-20200203))))

(: pair-with-age (((Listof Path)) (Date) . ->* . (Listof AgePathPair)))
;; pair age (or generation) with path
(define (pair-with-age paths [reference-date (gg:now)])
  (map (lambda ([path : Path]) `(,(backup-age-in-months path reference-date) ,path)) paths))

(module+ test #| sort by age |#
  (check-equal? (sort-by-age `((5 ,valid-path-20200201) (4 ,valid-path-20200203)))
                `((4 ,valid-path-20200203) (5 ,valid-path-20200201) )))

;; module with untyped definitions that would not typecheck
(module UNTYPED racket/base
  (define (sort-by-age age-path-pairs)
    (sort age-path-pairs < #:key (lambda (pair) (car pair)))) ;; put into untyped region since type checker cannot work with polymorphic key-word parameter (racket 7.5)
  (provide sort-by-age))

(require/typed 'UNTYPED
  [sort-by-age ((Listof AgePathPair) -> (Listof AgePathPair)) ])

(module+ test #| fill gaps |#
  (check-equal? (--fill-gaps `((2 ,a)(3 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,b) (3 ,b) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((0 ,a)(2 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,b) (3 ,b) (2 ,b) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((1 ,a)(5 ,b)(7 ,c)) 0 '())
                `((7 ,c) (6 ,b) (5 ,b) (4 ,a) (3 ,a) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((2 ,a)(5 ,b)) 0 '())
                `((5 ,b) (4 ,a) (3 ,a) (2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps `((2 ,a)) 0 '())
                `((2 ,a) (1 ,a) (0 ,a)))
  (check-equal? (--fill-gaps '() 0 '())
                '()))

(: --fill-gaps : (Listof AgePathPair) Nonnegative-Integer (Listof AgePathPair) -> (Listof AgePathPair))
;; fill missing generations (months) in list such that each generation is present exactly once (up to the max)
(define (--fill-gaps remaining-age-path-pairs n current-result)
  (cond [(empty? remaining-age-path-pairs)
         current-result]
        [(and (= 1 (length remaining-age-path-pairs))
              (> n (first (first remaining-age-path-pairs))))
         current-result]
        [(or (= 1 (length remaining-age-path-pairs))
             (< n (first (second remaining-age-path-pairs))))
         (--fill-gaps remaining-age-path-pairs
                     (add1 n)
                     (cons `(,n ,(second (first remaining-age-path-pairs))) current-result))]
        [(>= n  (first (second remaining-age-path-pairs)))
         (--fill-gaps (cdr remaining-age-path-pairs)
                     (add1 n)
                     (cons `(,n ,(second (second remaining-age-path-pairs))) current-result))]
        [else current-result]))

(module+ test #| fill gaps |#
  (check-equal? (fill-gaps '())
                '())
  (check-equal? (fill-gaps `((0 ,valid-path-20200101)))
                `((0 , valid-path-20200101)))
  (check-equal? (fill-gaps `((2 ,valid-path-20200101)))
                `((0 , valid-path-20200101)
                  (1 , valid-path-20200101)
                  (2 , valid-path-20200101)))
  (check-equal? (fill-gaps `((1 ,valid-path-20200514) (4 ,valid-path-20200203) (6 ,valid-path-20200101)))
                `((0 ,valid-path-20200514)
                  (1 ,valid-path-20200514)
                  (2 ,valid-path-20200514)
                  (3 ,valid-path-20200514)
                  (4 ,valid-path-20200203)
                  (5 ,valid-path-20200203)
                  (6 ,valid-path-20200101))))

(: fill-gaps : (Listof AgePathPair) -> (Listof AgePathPair))
;; convenience function for --fill-gaps
(define (fill-gaps sorted-age-path-pairs)
  (reverse (--fill-gaps sorted-age-path-pairs 0 '())))

(module+ test #| fib |#
  (check-equal? (fib 0) 0)
  (check-equal? (fib 1) 1)
  (check-equal? (fib 2) 1)
  (check-equal? (fib 3) 2)
  (check-equal? (fib 10) 55))

(: fib : Nonnegative-Integer -> Nonnegative-Integer)
(define (fib n)
  (cond [(= n 0) 0]
        [(< n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(module+ test #| fib backup ages to keep |#
  (check-true (set-member? fib-backup-ages-to-keep (fib 10)))
  (check-true (set-member? fib-backup-ages-to-keep 0))
  (check-false (set-member? fib-backup-ages-to-keep (- (fib 10) 1))))

(: fib-backup-ages-to-keep (Setof Nonnegative-Integer))
;; set of ages / generations perspectively kept
(define fib-backup-ages-to-keep (list->set (map fib (range 0 15))))

(: keep-backup-since-age-is-kept? : (Setof Nonnegative-Integer) AgePathPair -> Boolean)
;; kept because of age / generation
(define (keep-backup-since-age-is-kept? backup-ages-to-keep age-backup-pair)
  (set-member? backup-ages-to-keep (first age-backup-pair)))

(module+ test #| age path pairs -> paths |#
  (check-equal? (age-path-pairs->paths `((0 ,a) (1 ,a) (2 ,b) (3 ,b) (4 ,c)))
                (set a b c)))

(: age-path-pairs->paths : (Listof AgePathPair) -> (Setof Path))
;; extract all paths from list of age path pairs
(define (age-path-pairs->paths age-path-pairs)
  (list->set (map (lambda ([pair : AgePathPair]) (second pair)) age-path-pairs)))

(module+ test #| next age ge |#
  (check-equal? (next-age-ge 3 (set 1 2 7 9)) 7)
  (check-equal? (next-age-ge 3 (set 1 2 4 9)) 4)
  (check-equal? (next-age-ge 3 (set 1 3 9)) 3)
  (check-equal? (next-age-ge 10 (set 1 3 9)) 10))

(: next-age-ge (([age : Nonnegative-Integer] [_ : (Setof Nonnegative-Integer)])
                . -> . (Refine [next-age : Nonnegative-Integer] (>= next-age age))))
;; get next age greater or equal within the list of backup ages to keep
(define (next-age-ge age backup-ages-to-keep)
  (define all-ge (filter (lambda ([fnum : Nonnegative-Integer]) (>= fnum age)) (set->list backup-ages-to-keep)))
  (define asc-sorted-ages (sort all-ge <))
  (define first-ge (if (empty? asc-sorted-ages) age (first asc-sorted-ages)))
  (if (>= first-ge age) first-ge age))

(define-type KeepFunction ((Listof Path) (Listof AgePathPair) -> (Setof Path)))

(: keep-first-n : Nonnegative-Integer (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep the first n paths from the given list of age path pairs
(define (keep-first-n n all-paths age-path-pairs)
  (list->set (take all-paths (min (length all-paths) n))))

(: keep-oldest : (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep the oldest path
(define (keep-oldest all-paths age-path-pairs)
  (set (cadr (last (sort-by-age age-path-pairs)))))

(: keep-by-age-list : (Setof Nonnegative-Integer) (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep all paths because generation / age is in list to keep
(define (keep-by-age-list backup-ages-to-keep all-paths age-path-pairs)
  (define filled-age-file-pairs (fill-gaps (sort-by-age age-path-pairs)))
  (age-path-pairs->paths (filter (curry keep-backup-since-age-is-kept? backup-ages-to-keep) filled-age-file-pairs)))

(module+ test #| keep because it becomes relevant |#
  (check-equal? (--keep-because-it-becomes-relevant `((1 ,a) (5 ,b) (7 ,c) (15 ,d)) 3 '() 6)
                `(,c ,d)))

(: --keep-because-it-becomes-relevant : (Listof AgePathPair) Nonnegative-Integer (Listof Path) Nonnegative-Integer -> (Listof Path))
;; keep because when oldest generation hits fib number, the given hits a fib number, too
(define (--keep-because-it-becomes-relevant sorted-age-path-pairs n kept-paths fib-distance)
  (cond [(= n 0) kept-paths]
        [else (define age-path-pair (list-ref sorted-age-path-pairs n))
              (define age           (first age-path-pair))
              (--keep-because-it-becomes-relevant
               sorted-age-path-pairs
               (sub1 n)
               (if (set-member? fib-backup-ages-to-keep (+ age fib-distance))
                   (cons (cadr age-path-pair) kept-paths)
                   kept-paths)
               fib-distance)]))

(module+ test #| keep because it becomes relevant |#
  (check-equal? (keep-because-it-becomes-relevant fib-backup-ages-to-keep (list a b c d) `((1 ,a) (5 ,b) (7 ,c) (15 ,d)))
                (set a c d)))

(: keep-because-it-becomes-relevant : (Setof Nonnegative-Integer) (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; sugar for --keep-because-it-becomes-relevant
(define (keep-because-it-becomes-relevant backup-ages-to-keep all-paths age-path-pairs)
  (define sorted-age-path-pairs     (fill-gaps (sort-by-age age-path-pairs)))
  (define oldest-age                (first (last sorted-age-path-pairs)))
  (define min-fib-older-than-oldest (next-age-ge oldest-age backup-ages-to-keep))
  (define n (length sorted-age-path-pairs))
  (cond [(> 1 n) (error "age-path-pairs must not be empty" age-path-pairs)]
        [else
         (list->set (--keep-because-it-becomes-relevant sorted-age-path-pairs
                                                      (sub1 n)
                                                      '()
                                                      (- min-fib-older-than-oldest oldest-age)))]))

(: backup-keep-functions (Listof KeepFunction))
;; list of predicates to apply when selecting full backups to keep
(define backup-keep-functions
  (list (curry keep-first-n 4)
        keep-oldest
        (curry keep-by-age-list fib-backup-ages-to-keep)
        (curry keep-because-it-becomes-relevant fib-backup-ages-to-keep)))

(module+ test #| keep paths |#
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

(: --kept-paths : (Listof Path) (Listof AgePathPair) (Setof Path) (Listof KeepFunction) -> (Setof Path))
;; apply keep functions to reduce the list of available backups
(define (--kept-paths all-paths age-path-pairs path-set keep-functions)
  (cond [(empty? keep-functions)
         path-set]
        [else
         (--kept-paths all-paths
                      age-path-pairs
                      (set-union path-set
                                 ((first keep-functions) all-paths age-path-pairs))
                      (rest keep-functions))]))

(module+ test #| keep paths |#
  (check-equal? (kept-paths (list a b c d e f g)
                            `((0 ,a) (1 ,b) (2 ,c) (3 ,d) (4 ,e) (5 ,f) (6 ,g)))
                (set a b c d f g)) ;; dropped e
  (check-equal? (kept-paths (list a b c d f g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g)))
                (set a b c d e g))
  (check-equal? (kept-paths (list a b c d e g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g) (13 ,g) (14 ,g) (15 ,h)))
                (set a b c d e g h))
  (check-equal? (kept-paths (list a b c d e g h)
                            (fill-gaps `((2 ,a) (3 ,b) (4 ,c)  (7 ,d) (8 ,e)  (9 ,f)  (12 ,g) (15 ,h))))
                (set a b c d e g h))

  (check-equal? (kept-paths (list l m n o p r u z)
                            (fill-gaps `((0 ,l) (1 ,m) (2 ,n) (3 ,o) (4 ,p) (6 ,r) (9 ,u) (14 ,z))))
                (set l m n o p r u z)) ;; drop none
  (check-equal? (kept-paths (list k l m n o p r u z)
                            (fill-gaps `((0 ,k) (1 ,l) (2 ,m) (3 ,n) (4 ,o) (5 ,p) (7 ,r) (10 ,u) (15 ,z))))
                (set k l m n p r u z)) ;; drop o
  (check-equal? (kept-paths (list j k l m n p r u z)
                            (fill-gaps `((0 ,j) (1 ,k) (2 ,l) (3 ,m) (4 ,n) (6 ,p) (8 ,r) (11 ,u) (16 ,z))))
                (set j k l m n r u z)) ;; drop p
  (check-equal? (kept-paths (list i j k l m n r u z)
                            (fill-gaps `((0 ,i) (1 ,j) (2 ,k) (3 ,l) (4 ,m) (5 ,n) (9 ,r) (12 ,u) (17 ,z))))
                (set i j k l m n r u z)) ;; drop none
  [check-equal? (kept-paths (list h i j k l m n r u z)
                            (fill-gaps `((0 ,h) (1 ,i) (2 ,j) (3 ,k) (4 ,l) (5 ,m) (6 ,n) (10 ,r) (13 ,u) (18 ,z))))
                (set h i j k m n r u z)] ;; drop l
  (check-equal? (kept-paths (list g h i j k m n r u z)
                            (fill-gaps `((0 ,g) (1 ,h) (2 ,i) (3 ,j) (4 ,k) (6 ,m) (7 ,n) (11 ,r) (14 ,u) (19 ,z))))
                (set g h i j k m n r z)) ;; drop u
  (check-equal? (kept-paths (list f g h i j k m n r z)
                            (fill-gaps `((0 ,f) (1 ,g) (2 ,h) (3 ,i) (4 ,j) (5 ,k) (7 ,m) (8 ,n) (12 ,r)(20 ,z))))
                (set f g h i j k m n r z)) ;; drop none
  (check-equal? (kept-paths (list e f g h i j k m n r z)
                            (fill-gaps `((0 ,e) (1 ,f) (2 ,g) (3 ,h) (4 ,i) (5 ,j) (6 ,k) (8 ,m) (9 ,n) (13 ,r)(21 ,z))))
                (set e f g h j m r z))  ;; drop i, k, n
  )

(: kept-paths : (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; syntactic sugar for --kept-paths
(define (kept-paths all-paths age-path-pairs)
  (--kept-paths all-paths age-path-pairs (set) backup-keep-functions))

(: get-full-related-to : Path String (Listof Path) -> (Listof Path))
;; get all files related to the following full-backup
(define (get-full-related-to sig-file backup-dir full-backup-files)
  (define date (regexp-replace #rx".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1"))
  (filter (lambda ([path : Path])
            (regexp-match (regexp (format ".*duplicity-full\\.~a\\..*" date)) (path->string path)))
          full-backup-files))

(module+ test #| get to datestr of chain |#
  (check-equal? (get-to-datestr-of-chain (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))
                "20191011T121221Z")
  (check-equal? (get-from-datestr-of-chain (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))
                "20191011T115918Z"))

(: get-to-datestr-of-chain : Path -> String)
;; get to date string from file name of incremental backup file
(define (get-to-datestr-of-chain manifest-file)
  (regexp-replace #rx".*duplicity-inc\\..*\\.to\\.(.*)\\.manifest\\..*" (path->string manifest-file) "\\1"))

(: get-from-datestr-of-chain : Path -> String)
;; get from date string from file name of incremental backup file
(define (get-from-datestr-of-chain manifest-file)
  (regexp-replace #rx".*duplicity-inc\\.(.*)\\.to\\..*\\.manifest\\..*" (path->string manifest-file) "\\1"))

(module+ test #| get increment based on |#
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

(: get-increment-based-on : Path (Listof Path) -> (Listof Path))
;; get files of the incremental backup based on the given manifest file of this increment
(define (get-increment-based-on manifest-file full-backup-files)
  (define from-date (get-from-datestr-of-chain manifest-file))
  (define to-date   (get-to-datestr-of-chain manifest-file))
  (filter (lambda ([path : Path])
            (regexp-match (regexp (format ".*duplicity-inc\\.~a\\.to\\.~a\\..*" from-date to-date)) (path->string path)))
          full-backup-files))

(module+ test #| get manifest based on |#
  (check-equal? (get-manifest-based-on
                 "20191011T115918Z"
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.gpg")))
                (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))

(: get-manifest-based-on : String (Listof Path) -> (U Path Void))
;; get manifest of incremental backup from date (string)
(define (get-manifest-based-on from-date-str full-backup-files)
  (define related-files (filter (lambda ([path : Path])
                                  (regexp-match (regexp (format ".*duplicity-inc\\.~a\\.to\\..*\\.manifest\\..*" from-date-str)) (path->string path)))
                                full-backup-files))
  (when (not (empty? related-files))
    (first related-files)))

(module+ test #| get all increment manifests of chain |#
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

(: --get-all-increment-manifests-of-chain : String (Listof Path) (Listof Path) -> (Listof Path))
;; get all manifest files of all incrementals within one full (chain) starting with from-date
(define (--get-all-increment-manifests-of-chain from-date full-backup-files collected-manifest-files)
  (let ([manifest (get-manifest-based-on from-date full-backup-files)])
    (if (path? manifest)
        (begin
          (--get-all-increment-manifests-of-chain (get-to-datestr-of-chain manifest) full-backup-files (cons manifest collected-manifest-files)))
        collected-manifest-files)))

(module+ test #| get chains related to |#
  (check-equal? (get-chains-related-to
                 (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20191011T115918Z.sigtar.gpg")
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

(: get-chains-related-to : Path (Listof Path) -> (Listof (Listof Path)))
;; get incrementals and files of those from the given full-backup
(define (get-chains-related-to sig-file full-backup-files)
  (define date                (regexp-replace #rx".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1"))
  (define all-chain-manifests (--get-all-increment-manifests-of-chain date full-backup-files '()))
  (map (lambda ([manifest : Path]) (get-increment-based-on manifest full-backup-files)) all-chain-manifests))

(module+ test #| unique-ages-path-pairs |#
  (check-equal?
   (--unique-ages-path-pairs (pair-with-age (list valid-path-20200502 ;; age 5, keep is fib # (used for 6, 7 too)
                                                 valid-path-20200514 ;; age 5, keep (four youngest)
                                                 valid-path-20200605 ;; age 4
                                                 valid-path-20200701)  ;; age 4
                                           (gg:date 2020 11 01))
                            (hash))
   (hash 4 valid-path-20200701 5 valid-path-20200514))
  (check-equal? (--unique-ages-path-pairs '() (hash))
                (hash)))

(: --unique-ages-path-pairs : (Listof AgePathPair) (Immutable-HashTable Nonnegative-Integer Path) -> (Immutable-HashTable Nonnegative-Integer Path))
;; keep only one path per generation/age
(define (--unique-ages-path-pairs rest-age-path-pairs current-result-pairs)
  (cond [(empty? rest-age-path-pairs)
         current-result-pairs]
        [else
         (define age (first (first rest-age-path-pairs)))
         (define path (second (first rest-age-path-pairs)))
         (--unique-ages-path-pairs (cdr rest-age-path-pairs) (hash-set current-result-pairs age path))]))

(module+ test #| unique ages path pairs |#
  (check-equal?
     (unique-ages-path-pairs (pair-with-age (list valid-path-20200502 ;; age 5, keep is fib # (used for 6, 7 too)
                                                 valid-path-20200514 ;; age 5, keep (four youngest)
                                                 valid-path-20200605 ;; age 4
                                                 valid-path-20200701)  ;; age 4
                                           (gg:date 2020 11 01)))

     (list (list 4 valid-path-20200701) (list 5 valid-path-20200514))))

(: unique-ages-path-pairs : (Listof AgePathPair) -> (Listof AgePathPair))
;; make sure onle one path is kept per generation/month
(define (unique-ages-path-pairs age-path-pairs)
  (define uniqified-map (--unique-ages-path-pairs age-path-pairs (hash)))
  (sort-by-age (map (lambda ([key : Nonnegative-Integer]) (list key (hash-ref uniqified-map key)) ) (hash-keys uniqified-map))))

(module+ test #| classify sigfiles |#
  (check-equal? (hash-ref (classify-sigfiles (list valid-path-20200101) (gg:date 2020 03 01))
                          'discard)
                (set))
  (check-equal? (backup-age-in-months valid-path-20200101 (gg:date 2020 11 01))
                10 )
  (check-equal? (backup-age-in-months valid-path-20200201 (gg:date 2020 11 01))
                9)
  (check-equal? (backup-age-in-months valid-path-20200203 (gg:date 2020 11 01))
                8)
  (check-equal? (backup-age-in-months valid-path-20200502 (gg:date 2020 11 01))
                5)
  (check-equal? (hash-ref (classify-sigfiles (list valid-path-20200101 ;; age 10, keep (oldest)
                                                   valid-path-20200201 ;; age 9, discard!
                                                   valid-path-20200203 ;; age 8, keep is fib #
                                                   valid-path-20200502 ;; sorted out (age 5)
                                                   valid-path-20200514 ;; age 5 (and 6, 7)  keep (four youngest)
                                                   valid-path-20200605
                                                   valid-path-20200701
                                                   valid-path-20201004)
                                             (gg:date 2020 11 01))
                          'discard)
                (set valid-path-20200201
                     valid-path-20200502)))

(: path>? : Path Path -> Boolean)
;; compare paths as strings
(define (path>? [p1 : Path] [p2 : Path])
  (string>? (path->string p1) (path->string p2)))

(: classify-sigfiles (((Listof Path)) (Date) . ->* . (Immutable-HashTable Symbol (Setof Path))))
;; classify sig files of full backups into 'keep or 'discard
(define (classify-sigfiles sigfiles [reference-date (gg:now)])
  (define sorted-sigfiles (sort sigfiles path>?))
  (define age-file-pairs  (sort-by-age (unique-ages-path-pairs (pair-with-age sigfiles reference-date))))
  (define keep            (kept-paths sorted-sigfiles age-file-pairs))
  (define discard         (set-subtract (list->set sigfiles) keep))
  (hash 'keep keep 'discard discard))

;; simple check and print of current kept and discarded backups (no actual actions taken)
(define (check-backups)
  (printf "locating configuration\n")
  (define config     (read-configuration (file->lines (build-path (find-system-path 'home-dir) ".duplicity/config"))))
  (define backup-dir (hash-ref config 'backup-folder))
  (cond [(directory-exists? backup-dir)
         (printf "dir ~s exists\n" backup-dir)
         (define full-backup-files   (directory-list backup-dir))
         (define full-sig-files      (filter matched-backup-file full-backup-files))
         (define classified-sigfiles (classify-sigfiles full-sig-files))
         (define sec-dump            (build-path (find-system-path 'home-dir) "temp"))
         (define discarded-dep-files (map (lambda ([path : Path]) (get-chains-related-to path full-backup-files)) (set->list (hash-ref classified-sigfiles 'discard))))
         (printf "keeping ~s\n" (hash-ref classified-sigfiles 'kept))
         (printf "discard ~s\n" (hash-ref classified-sigfiles 'discard))
         (printf "discard along with sigfile, depended files: ~s\n" discarded-dep-files)
         (for-each (lambda ([path : Any])
                     (when (path? path)
                       (begin
                         (printf "moving discarded file ~s to ~s" path sec-dump)
                         ;; (rename-file-or-directory path sec-dump)
                         )))
                   (flatten discarded-dep-files))]
        [else
         (printf "dir ~s does not exist\n" backup-dir)]))

;; (check-backups)

;; (printf "Given arguments: ~s\n"
;;         (current-command-line-arguments))
