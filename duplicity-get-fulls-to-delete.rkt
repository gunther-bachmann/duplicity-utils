#! /usr/bin/env racket
#lang typed/racket #:with-refinements

;; find out which months/generation/age can be deleted from backup given the following:
;; - full backups are done monthly, incrementals sub monthly
;; - keep a number (4) of most recent full
;; - keep the oldest
;; - keep all with a months age matching a fibonacci number
;; - keep those that will (in time) have a months age matching a fibonacci number,
;;   when the oldest backup matches a fibonacci number
;; - a backup is associated with a generation/age if there is no backup older than this one
;;   that is less or equal to that number

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

;; matches full backups by one signature file
(define full-backup-ls-pattern : Regexp #rx"duplicity-full-signatures\\..*\\.sigtar\\.gpg$")

;; configuration known and processed when reading the configuration
(define known-config-keys : (HashTable String Boolean) (hash "backup-folder" #t "temp-folder" #t))

(module+ test #| process config line |#
  (check-false (hash-has-key? (process-config-line "some: value" (hash)) 'some))
  (check-true (hash-has-key? (process-config-line "backup-folder: value" (hash)) 'backup-folder))
  (check-false (hash-has-key? (process-config-line "#backup-folder: value" (hash)) 'backup-folder))
  (check-false (hash-has-key? (process-config-line "backup-folder : value" (hash)) 'backup-folder))
  (check-false (hash-has-key? (process-config-line " backup-folder: value" (hash)) 'backup-folder))
  (check-equal? (hash-ref (process-config-line "backup-folder:value" (hash)) 'backup-folder)
                "value")
  (check-equal? (hash-ref (process-config-line "backup-folder:       value" (hash)) 'backup-folder)
                "value")
  (check-equal? (hash-ref (process-config-line "backup-folder: value" (hash)) 'backup-folder)
                "value")
  (check-equal? (hash-ref (process-config-line "backup-folder: /some/path/to/backup" (hash)) 'backup-folder)
                "/some/path/to/backup"))

(: process-config-line : String Configuration -> Configuration)
;; process one configuration line and return enriched configuration
(define (process-config-line line configuration)
  (define matched (regexp-match #rx"^([^# ][^:]*): *(.*)" line))
  (if matched
      (let ([key   (assert (list-ref (cdr matched) 0) string?)] ;; regexp could return this to be #f
            [value (assert (list-ref (cdr matched) 1) string?)]) ;; same ...
        (if (hash-has-key? known-config-keys key)
            (hash-set configuration (string->symbol key) value)
            configuration))
      configuration))

;; seed
(define empty-config-hash : Configuration (hash))

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
;; pair path with age (or generation)
(define (pair-with-age paths [reference-date (gg:now)])
  (map (lambda ([path : Path]) (list (backup-age-in-months path reference-date) path))
       paths))

(module+ test #| sort by age |#
  (check-equal? (sort-by-age `((5 ,valid-path-20200201) (4 ,valid-path-20200203)))
                `((4 ,valid-path-20200203) (5 ,valid-path-20200201) )))

;; module with untyped definitions that would not typecheck
(module UNTYPED racket/base
  (define (sort-by-age age-path-pairs)
    (sort age-path-pairs < #:key (lambda (pair) (car pair)))) ;; put into untyped region since type checker cannot work with polymorphic key-word parameter (racket <=7.5)
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
        [(>= n (first (second remaining-age-path-pairs)))
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
        [(<= n 2) 1]
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
  (list->set (map (lambda ([pair : AgePathPair]) (second pair))
                 age-path-pairs)))

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
(define (keep-first-n n all-paths _)
  (list->set (take all-paths (min (length all-paths) n))))

(: keep-oldest : (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep the oldest path
(define (keep-oldest _ age-path-pairs)
  (set (cadr (last (sort-by-age age-path-pairs)))))

(: keep-by-age-list : (Setof Nonnegative-Integer) (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep all paths because generation / age is in list to keep
(define (keep-by-age-list backup-ages-to-keep _ age-path-pairs)
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

(module+ test #| get full related to |#
  (check-equal? (get-full-related-to
                 (string->path "duplicity-full-signatures.20191011T115918Z.sigtar.gpg")
                 `(,(string->path "duplicity-full.20191011T115918Z.manifest.gpg")
                   ,(string->path "duplicity-full.20191011T115918Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-full.20191011T115918Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-full-signatures.20191011T115918Z.sigtar.gpg")
                   ,(string->path "duplicity-full.20191111T093003Z.manifest.gpg")
                   ,(string->path "duplicity-full.20191111T093003Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-full-signatures.20191111T093003Z.sigtar.gpg")))
                `(,(string->path "duplicity-full.20191011T115918Z.manifest.gpg")
                   ,(string->path "duplicity-full.20191011T115918Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-full.20191011T115918Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-full-signatures.20191011T115918Z.sigtar.gpg"))))

(: get-full-related-to : Path (Listof Path) -> (Listof Path))
;; get all files related to the following full-backup
(define (get-full-related-to sig-file full-backup-files)
  (define date (regexp-replace #rx"duplicity-full-signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1"))
  (define full-regexp (regexp (format "duplicity-full(-signatures)?\\.~a\\.(manifest|vol[0-9]+\\.difftar|sigtar)\\.gpg" date)))
  (filter (lambda ([path : Path])
            (regexp-match full-regexp (path->string path)))
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
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-new-signatures.20191011T115918Z.to.20191011T121221Z.sigtar.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-new-signatures.20191011T121221Z.to.20191012T121221Z.sigtar.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.difftar.gpg")))
                `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")
                  ,(string->path "duplicity-new-signatures.20191011T115918Z.to.20191011T121221Z.sigtar.gpg"))))

(: get-increment-based-on : Path (Listof Path) -> (Listof Path))
;; get files of the incremental backup based on the given manifest file of this increment
(define (get-increment-based-on manifest-file full-backup-files)
  (define from-date (get-from-datestr-of-chain manifest-file))
  (define to-date   (get-to-datestr-of-chain manifest-file))
  (define increment-regexp (regexp (format "duplicity-(inc|new-signatures)\\.~a\\.to\\.~a\\.(manifest|vol[0-9]+\\.difftar|sigtar)\\.gpg" from-date to-date)))
  (filter (lambda ([path : Path])
            (regexp-match increment-regexp (path->string path)))
          full-backup-files))

(module+ test #| get manifest based on |#
  (check-true (void? (get-manifest-based-on "" '())))
  (check-true (void? (get-manifest-based-on "some" '())))
  (check-true (void? (get-manifest-based-on "2019" `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))))
  (check-true (void? (get-manifest-based-on "20191011T115918Z" `(,(string->path "duplicity-inc.20191011T115918Z.t.20191011T121221Z.manifest.gpg")))))
  (check-true (void? (get-manifest-based-on "20191011T121221Z" `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))))
  (check-true (path? (get-manifest-based-on "20191011T115918Z" `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))))
  (check-equal? (get-manifest-based-on
                 "20191011T115918Z"
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.difftar.gpg")))
                (string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")))

(: get-manifest-based-on : String (Listof Path) -> (U Path Void))
;; get manifest of incremental backup from date (string)
(define (get-manifest-based-on from-date-str full-backup-files)
  (define dupl-regexp (regexp (format "duplicity-inc\\.~a\\.to\\..*\\.manifest\\.gpg" from-date-str)))
  (define related-files (memf (lambda ([path : Path])
                                  (begin
                                    (regexp-match dupl-regexp
                                                  (path->string path))))
                                full-backup-files))
  (when related-files
    (first related-files)))

(module+ test #| get all increment manifests of chain |#
  (check-equal? (--get-all-increment-manifests-of-chain
                 "20191011T115918Z"
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.difftar.gpg"))
                 '())
                `(,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                  ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg"))))

(: --get-all-increment-manifests-of-chain : String (Listof Path) (Listof Path) -> (Listof Path))
;; get all manifest files of all incrementals within one full (chain) starting with from-date
(define (--get-all-increment-manifests-of-chain from-date full-backup-files collected-manifest-files)
  (define manifest (get-manifest-based-on from-date full-backup-files))
  (if (path? manifest)
      (--get-all-increment-manifests-of-chain (get-to-datestr-of-chain manifest)
                                             full-backup-files
                                             (cons manifest collected-manifest-files))
      collected-manifest-files))

(module+ test #| get chains related to |#
  (check-equal? (get-chains-related-to
                 (string->path "/run/media/myself/harddrive/data-backup/duplicity-full-signatures.20191011T115918Z.sigtar.gpg")
                 `(,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.difftar.gpg")))
                `((,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T121221Z.to.20191012T121221Z.vol1.difftar.gpg"))
                  (,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.manifest.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol1.difftar.gpg")
                   ,(string->path "duplicity-inc.20191011T115918Z.to.20191011T121221Z.vol2.difftar.gpg")))))

(: get-chains-related-to : Path (Listof Path) -> (Listof (Listof Path)))
;; get incrementals and files of those from the given full-backup
(define (get-chains-related-to sig-file full-backup-files)
  (define date                (regexp-replace #rx".*signatures\\.(.*)\\.sigtar.gpg" (path->string sig-file) "\\1"))
  (define all-chain-manifests (--get-all-increment-manifests-of-chain date full-backup-files '()))
  (map (lambda ([manifest : Path])
         (get-increment-based-on manifest full-backup-files))
       all-chain-manifests))

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
  (sort-by-age (map (lambda ([key : Nonnegative-Integer]) (list key (hash-ref uniqified-map key)))
                    (hash-keys uniqified-map))))

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

(module+ test #| age-path-pairs->ages |#
  (check-equal? (age-path-pairs->ages (list (list 1 (string->path "some"))))
                (list 1))
  (check-equal? (age-path-pairs->ages (list (list 1 (string->path "some")) (list 2 (string->path "other"))))
                (list 1 2))
  (check-equal? (age-path-pairs->ages (list))
                (list)))

(: age-path-pairs->ages : (Listof AgePathPair) -> (Listof Nonnegative-Integer))
;; get only ages from age path pairs
(define (age-path-pairs->ages age-path-pairs)
  (map (lambda ([pair : AgePathPair]) (first pair)) age-path-pairs))

(: --validated-intervals : (Listof Nonnegative-Integer) (Setof Nonnegative-Integer) -> (Setof Nonnegative-Integer))
(define (--validated-intervals sorted-ages result)
  (cond [(empty? sorted-ages)
         result]
        [(= 1 (length sorted-ages))
         (set-add result (next-age-ge (car sorted-ages) fib-backup-ages-to-keep))]
        [else
         (define backup-n (first sorted-ages))
         (define backup-o (second sorted-ages))
         (define backup-n-ni (next-age-ge backup-n fib-backup-ages-to-keep))
         (define backup-o-ni (next-age-ge backup-o fib-backup-ages-to-keep))
         (define new-result (if (>= (- backup-o-ni backup-n-ni)
                                   (- backup-o backup-n))
                                (set-add result backup-n-ni)
                                result))
         (--validated-intervals (cdr sorted-ages) new-result)]))

(module+ test #| validated-intervals |#
  (check-equal? (validated-intervals '(0 1 2 3 4 5 6 7 8 9 10))
                (set 0 1 2 3 5 8 13))
  (check-equal? (validated-intervals '(0 1 2 3 5 8 13))
                (set 0 1 2 3 5 8 13))
  (check-equal? (validated-intervals '())
                (set))
  (check-equal? (validated-intervals '(0 10))
                (set 0 13))
  (check-equal? (validated-intervals '(0 4))
                (set 0 5))
  (check-equal? (validated-intervals '(0 3 4 8)) ;; 4 does not cover interval 4-5, since between 4 and 8 is a larger distance, than it should be (should be max: 3 [8-5])
                (set 0 3 8))
  (check-equal? (validated-intervals '(0 3 5 7))
                (set 0 3 5 8))
  (check-equal? (validated-intervals '(0 3 5 7 13)) ;; 7 does not cover interval 6-8, since between 7 and 13 is a larger dinstance, than it should be (should be max 5 [13-8])
                (set 0 3 5 13)))

(: validated-intervals : (Listof Nonnegative-Integer) -> (Setof Nonnegative-Integer))
;; get the valid/satisfied intervalls by fib-numbers of the given backup ages, taking distances between backups into account
(define (validated-intervals sorted-ages)
  (--validated-intervals sorted-ages (set)))

(: get-validated-intervals (((Listof Path)) (Date) . ->* . (Setof Nonnegative-Integer)))
;; classify sig files of full backups into 'keep or 'discard
(define (get-validated-intervals sigfiles [reference-date (gg:now)])
  (define sorted-sigfiles (sort sigfiles path>?))
  (define age-file-pairs  (sort-by-age (unique-ages-path-pairs (pair-with-age sigfiles reference-date))))
  (define ages            (age-path-pairs->ages age-file-pairs))
  (validated-intervals ages))

(module+ test #| unsatisfied-intervals |#
  (check-equal? (unsatisfied-intervals '(0 1 2 3 4 5 6 7 8 9 10))
                (set))
  (check-equal? (unsatisfied-intervals '(0 1 2 3 5 8 13))
                (set))
  (check-equal? (unsatisfied-intervals '())
                (set))
  (check-equal? (unsatisfied-intervals '(0 10))
                (set 1 2 3 5 8))
  (check-equal? (unsatisfied-intervals '(0 4))
                (set 1 2 3))
  (check-equal? (unsatisfied-intervals '(0 3 4 8)) ;; 4 does not cover interval 4-5, since between 4 and 8 is a larger distance, than it should be (should be max: 3 [8-5])
                (set 1 2 5))
  (check-equal? (unsatisfied-intervals '(0 3 5 7))
                (set 1 2))
  (check-equal? (unsatisfied-intervals '(0 3 5 7 13)) ;; 7 does not cover interval 6-8, since between 7 and 13 is a larger dinstance, than it should be (should be max 5 [13-8])
                (set 1 2 8)))

(: unsatisfied-intervals : (Listof Nonnegative-Integer) -> (Setof Nonnegative-Integer))
;; return the fib-numbers of the intervals that are not satisfied by the given backup ages, taking distances between backups into account
(define (unsatisfied-intervals sorted-ages)
  (cond [(empty? sorted-ages)
         (set)]
        [else
         (define valid-intervals (validated-intervals sorted-ages))
         (define max-interval (apply max (set->list valid-intervals)))
         (set-filter (set-subtract fib-backup-ages-to-keep (validated-intervals sorted-ages))
                     (lambda ([n : Nonnegative-Integer]) (<= n max-interval)))]))

(: --set-filter (All (A) (-> (Setof A) (Setof A) (-> A Boolean) (Setof A))))
(define (--set-filter unfiltered-set filtered-set predicate)
  (cond [(set-empty? unfiltered-set)
         filtered-set]
        [else
         (define new-filtered-set (if (predicate (set-first unfiltered-set))
                                      (set-add filtered-set (set-first unfiltered-set))
                                      filtered-set))
         (--set-filter (set-rest unfiltered-set) new-filtered-set predicate)]))

(module+ test #| set-filter |#
  (check-equal? (set-filter (set 1 2 3) (lambda ([x : Integer]) (zero? (modulo x 2))))
                (set 2))
  (check-equal? (set-filter (set "a" "bc" "d") (lambda ([x : String]) (< 1 (string-length x))))
                (set "bc")))

(: set-filter (All (A) (-> (Setof A) (-> A Boolean) (Setof A))))
;; filter the given set, keeping only values that satisfy the predicate
(define (set-filter unfiltered-set predicate)
  (--set-filter unfiltered-set (set-subtract unfiltered-set unfiltered-set) predicate))

;; simple check and print of current kept and discarded backups (no actual actions taken)
(define (check-backups)
  (printf "locating configuration\n")
  (define config     (read-configuration (file->lines (build-path (find-system-path 'home-dir) ".duplicity/config"))))
  (define backup-dir (hash-ref config 'backup-folder))
  (cond [(directory-exists? backup-dir)
         (printf "dir ~s exists\n" backup-dir)
         (define full-backup-files   (directory-list backup-dir))
         (printf "read full backup list\n")
         (define full-sig-files      (filter matched-backup-file full-backup-files))
         (printf "read signature files\n")
         (define classified-sigfiles (classify-sigfiles full-sig-files))
         (printf "classified signature files\n")
         (define sec-dump            (hash-ref config 'temp-folder))
         (define discarded-dep-files (map (lambda ([path : Path])
                                            (get-chains-related-to path full-backup-files))
                                          (set->list (hash-ref classified-sigfiles 'discard))))
         (define discarded-full-files (map (lambda ([path : Path])
                                             (get-full-related-to path full-backup-files))
                                           (set->list (hash-ref classified-sigfiles 'discard))))
         (printf "\ncollected complete list of files to discard\n")
         (define intervals           (get-validated-intervals full-sig-files))
         (printf "calculated validated intervals before deletion\n")
         (define intervals-after     (get-validated-intervals (set->list (hash-ref classified-sigfiles 'keep))))
         (printf "validated intervals ~s\n" intervals)
         (printf "validated intervals heeding deletion ~s\n" intervals-after)
         (printf "keep ~s\n" (hash-ref classified-sigfiles 'keep))
         (printf "discard ~s\n" (hash-ref classified-sigfiles 'discard))
         (printf "discard along with sigfile, dependend files: ~a\n" (+ (length (flatten discarded-full-files)) (length (flatten discarded-dep-files))))
         (cond [(and (set=? intervals intervals-after)
                     (not (set-empty? (hash-ref classified-sigfiles 'discard))))
                (printf "Discarding superfluous backups ...\n")
                (for-each (lambda ([path : Any])
                            (when (path? path)
                              (begin
                                (printf "moving discarded file ~s to ~s\n" (path->string path) sec-dump)
                                (rename-file-or-directory (build-path backup-dir path) (build-path sec-dump path))
                                )))
                          (append (flatten discarded-full-files) (flatten discarded-dep-files)))]
               [(not (set=? intervals intervals-after))
                (printf "WARNING: Not doing anything, since interval would be incomplete.\n")]
               [else
                (printf "Found nothing to discard.")])]
        [else
         (printf "dir ~s does not exist\n" backup-dir)]))

;; (check-backups)

;; (printf "Given arguments: ~s\n"
;;         (current-command-line-arguments))
