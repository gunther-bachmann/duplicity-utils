#! /usr/bin/env racket
#lang typed/racket #:with-refinements

;; find out which months/generation/age can be deleted from backup given the following:
;; - full backups are done monthly, incrementals sub monthly
;; - given three backups b_i-1, b_i, b_i+1, with two of them covering an interval iv_n, b_i is dropped,
;;   iff b_i+1 - b_i-1 < fib(n)

;; automatic installation of racket packages is problematic:
;; problem is that no racket packages can be installed into the racket-minimal installation used by shell-nix
;; #! /usr/bin/env nix-shell
;; #! nix-shell -i racket -p racket-minimal


;; glossary:
;;   backup                       :: a backup (b_i = age) is identified by its age in months and its signature files in the backup
;;   sigfile alias signature file :: is a unique path to (one element of) the monthly full backup
;;   age                          :: each backup is of a certain age that is, months old from now (or the given reference date)
;;   interval                     :: an interval (0..) is a time span of 1+ months, that grows in length using fibonacci numbers
;;                                  iv_0 = month 0
;;                                  iv_1 = month 1
;;                                  iv_2 = month 2
;;                                  iv_3 = month 3,4
;;                                  iv_4 = month 5,6,7
;;                                  iv_5 = month 8,9,10,11,12
;;                                  ...In = (range (quick-fib (+ 1 n)) (- (quick-fib (+ 2 n)) 1))
;;   interval length              :: is the length of the interval in months, (len iv_n) = (quick-fib n)
;;   interval coverage            :: an interval is covered iff a backup of age a exists that is within the interval

;; import gregorian dates with typing
(require/typed (prefix-in gg: gregor)
  [#:opaque gg:Date gg:date?]
  [#:opaque gg:Datetime gg:datetime?]
  [gg:parse-date (String String -> gg:Date)]
  [gg:date=? ((U gg:Date gg:Datetime) (U gg:Date gg:Datetime) -> Boolean)]
  [gg:date ((Integer) (Integer Integer) . ->* . gg:Date)]
  [gg:now (-> gg:Datetime)]
  [gg:current-posix-seconds (-> Real)])

;; import gregorian periods with typing
(require/typed (prefix-in gg: gregor/period)
  [#:opaque Period gg:period?]
  [gg:period-between (Date Date (Listof Symbol) -> Period)]
  [gg:period-ref (Period Symbol -> Integer)])

(require racket/cmdline)
(require/typed ansi-color
  [with-colors ((U Symbol String) (U Symbol String) (-> Void) -> Void)]
  [background-color (-> String)])

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

(: filter-config-lines : (Listof String) String -> (Listof String))
;; get lines of this profile only
(define (filter-config-lines lines profile)
  (define at-profile (dropf lines (lambda ([line : String]) (not (equal? (format "[~a]" profile) (string-trim line))))))
  (if (empty? at-profile)
      '()
      (takef (drop at-profile 1) (lambda ([line : String]) (not (string-prefix? line "[")) ))))

(module+ test #| filter-config-lines |#
  (check-equal? (filter-config-lines '() "profile")
                '())
  (check-equal? (filter-config-lines '("line 1"
                                       "line 2"
                                       "# comment"
                                       "[profile]"
                                       "relevant line"
                                       "[next profile]"
                                       "irrelevant line")
                                     "unknown profile")
                '())
  (check-equal? (filter-config-lines '("line 1"
                                       "line 2"
                                       "# comment"
                                       "[profile]"
                                       "relevant line"
                                       "[next profile]"
                                       "irrelevant line")
                                     "profile")
                '("relevant line"))
    (check-equal? (filter-config-lines '("line 1"
                                       "line 2"
                                       "# comment"
                                       "[profile]"
                                       "irrelevant line"
                                       "[next profile]"
                                       "relevant line")
                                     "next profile")
                '("relevant line")))

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
  (check-equal? (read-configuration '("[profile]"
                                      "backup-folder: some/folder"
                                      "# some comment"
                                      "unknown-prefix: ignored")
                                    "profile")
                (hash 'backup-folder "some/folder")))

(: read-configuration : (Listof String) String -> Configuration)
;; duplicity configuration
(define (read-configuration file-lines profile)
  (foldl (lambda ([arg : String] [acc : Configuration])
           (process-config-line arg acc))
         empty-config-hash
         (filter-config-lines file-lines profile)))

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

(module+ test #| fib |#
  (check-equal? (fib 0) 0)
  (check-equal? (fib 1) 1)
  (check-equal? (fib 2) 1)
  (check-equal? (fib 3) 2)
  (check-equal? (fib 10) 55))

(: fib : Nonnegative-Integer -> Nonnegative-Integer)
;; recursive calc fibonacci number
(define (fib n)
  (cond [(= n 0) 0]
        [(<= n 2) 1]
        [else (+ (fib (- n 1)) (fib (- n 2)))]))

(module+ test #| age path pairs -> paths |#
  (check-equal? (age-path-pairs->paths `((0 ,a) (1 ,a) (2 ,b) (3 ,b) (4 ,c)))
                (set a b c)))

(: age-path-pairs->paths : (Listof AgePathPair) -> (Setof Path))
;; extract all paths from list of age path pairs
(define (age-path-pairs->paths age-path-pairs)
  (list->set (map (lambda ([pair : AgePathPair]) (second pair))
                 age-path-pairs)))

(define-type KeepFunction ((Listof Path) (Listof AgePathPair) -> (Setof Path)))

(: keep-first-n : Nonnegative-Integer (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep the first n paths from the given list of age path pairs
(define (keep-first-n n all-paths _)
  (list->set (take all-paths (min (length all-paths) n))))

(define golden-ratio (/  (+ 1 (sqrt 5) ) 2))

(: inverse-fib : Nonnegative-Integer -> Nonnegative-Integer)
;; inverse fibonacci number (without recursion)
;; see https://en.wikipedia.org/wiki/Fibonacci_number#Relation_to_the_golden_ratio
(define (inverse-fib n)
  (cond ((> n (quick-fib 100)) (raise-user-error 'unsafe-value-domain))
        ((> n 1)
         (begin
           (define result
             (exact-floor
              (real-part
               (/ (log (+ (* n (sqrt 5)) (/ 1 2))) (log golden-ratio)))))
           (if (>= result 0) ;; satisfy type checker, result cannot become less than 0
               result
               0)))
        ((= n 1) 1)
        (else 0)))

(: quick-fib : Nonnegative-Integer -> Nonnegative-Integer)
;; calc fibonacci number without recursion, given the floating point precision exact at least up to 30 (which is enough for this use case)
(define (quick-fib n)
  (cond ((> n 100) (raise-user-error 'unsafe-value-domain))
        ((> n 0)
         (begin
           (define result
             (exact-floor
              (real-part
               (+ (/ (expt golden-ratio n) (sqrt 5)) (/ 1 2)))))
           (if (>= result 0) ;; to satisfy type checker, which cannot analyze to formula above
               result
               0)))
        (else 0)))

(module+ test #| quick-fib, inverse-fib |#
  (check-equal? (map quick-fib (range 30))
                (map fib (range 30)))
  (check-equal? (map inverse-fib (map quick-fib (range 3 100))) ;; before 3, fib inverse-fib is not bijective
                (range 3 100)))

(: interval-num-of : Nonnegative-Integer -> Nonnegative-Integer)
;; get the interval number n of a backup of age a
(define (interval-num-of age)
  (cond ((> age 1)
         (begin
           (define result
             (- (inverse-fib age) 1))
           (if (>= result 0)
               result
               0)))
        (else age)))

(module+ test #| interval-num-of |#
  (check-equal? (map interval-num-of
                     '(0             ;; I0
                       1             ;; I1
                       2             ;; I2
                       3 4           ;; I3
                       5 6 7         ;; I4
                       8 9 10 12     ;; I5
                       13 14 19 20   ;; I6
                       21 22))       ;; I7
                '(0         ;; I0
                  1         ;; I1
                  2         ;; I2
                  3 3       ;; I3
                  4 4 4     ;; I4
                  5 5 5 5   ;;
                  6 6 6 6   ;;
                  7 7)))

(: drop-i-in-triplet? : Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer -> Boolean)
;; is it safe to drop backup with index i of the given triple of ages of backups with indexes i-1, i, i+1?
(define (drop-i-in-triplet? age-im1 age-i age-ip1)
  (define interval-im1 (interval-num-of age-im1))
  (define interval-i   (interval-num-of age-i))
  (define interval-ip1 (interval-num-of age-ip1))
  (cond ((and (equal? interval-im1 interval-i)
            (equal? interval-i interval-ip1))
         #t)
        ((or (equal? interval-im1 interval-i)
            (equal? interval-i interval-ip1))
         (< (- age-ip1 age-im1)
            (interval-length interval-i)))
        (else #f)))

(module+ test #| drop-i-in-triplet? |#
  (check-true (drop-i-in-triplet? 5 6 7)) ;; all in interval 4
  (check-true (drop-i-in-triplet? 10 11 12)) ;; all in interval 5
  (check-false (drop-i-in-triplet? 0 1 2)) ;; all in different intervals
  (check-false (drop-i-in-triplet? 3 5 8)) ;; all in different intervals

  (check-true (drop-i-in-triplet? 5 8 9)) ;; 5 enters interval 5 faster than 9 leaves it
  )

(: interval-length : Nonnegative-Integer -> Positive-Integer)
;; length (number of months) in the given interval
(define (interval-length interval-index)
  (cond ((= 0 interval-index) 1)
        (else (begin
                (define result (quick-fib interval-index))
                (if (>= result 1)
                    result
                    1)))))

(module+ test #| interval-length |#
  (check-equal? (interval-length 0)
                1)
  (check-equal? (interval-length 1)
                1)
  (check-equal? (interval-length 2)
                1)
  (check-equal? (interval-length 3)
                2)
  (check-equal? (interval-length 4)
                3)
  (check-equal? (interval-length 5)
                5))

(: -keep-by-triplet-distance : (Listof AgePathPair) AgePathPair AgePathPair AgePathPair (Setof Path) -> (Setof Path))
;; if api of the triplet can be deleted,
;;  (a) if there are remaining backups to fill up the triplet, take the next one off the remaining list and call recursively
;;  (b) if there is no remain backup, put apip1 and apim1 onto the kept paths and return
;; if api of the triplet CANNOT be deleted
;;  (a) if there are remaining backups to fill up the triplet, put aip1 into the list of kept paths, take the next one off the remaining list and call recursively
;;  (b) if there is no remaining backup, put apip1 api and apim1 onto the kept paths and return
(define (-keep-by-triplet-distance remaining-backup-age-pairs apim1 api apip1 kept-paths)
  (if (drop-i-in-triplet? (first apim1) (first api) (first apip1))
      (if (empty? remaining-backup-age-pairs)
          (set-add (set-add kept-paths (second apip1)) (second apim1))
          (-keep-by-triplet-distance (drop remaining-backup-age-pairs 1) (car remaining-backup-age-pairs) apim1 apip1 kept-paths))
      (if (empty? remaining-backup-age-pairs)
          (set-add (set-add (set-add kept-paths (second apip1)) (second apim1)) (second api))
          (-keep-by-triplet-distance (drop remaining-backup-age-pairs 1) (car remaining-backup-age-pairs) apim1 api (set-add kept-paths (second apip1))))))

(module+ test #| keep-by-triplet-distance |#
  (check-equal? (-keep-by-triplet-distance '() `(2 ,b) `(3 ,c) `(4 ,d) (set))
                (set b c d))
  (check-equal? (-keep-by-triplet-distance '() `(2 ,b) `(3 ,c) `(4 ,d) (set e f))
                (set b c d e f))
  (check-equal? (-keep-by-triplet-distance `((1 ,a) (2 ,b)) `(3 ,c) `(4 ,d) `(5 ,e) (set f))
                (set a b c d e f))
  (check-equal? (-keep-by-triplet-distance `((1 ,a)) `(2 ,b) `(3 ,c) `(4 ,d) (set))
                (set a b c d))
  (check-equal? (-keep-by-triplet-distance `((1 ,a)) `(5 ,b) `(6 ,c) `(7 ,d) (set))
                (set a b d))
  (check-equal? (-keep-by-triplet-distance `((5 ,a)) `(6 ,b) `(7 ,c) `(22 ,d) (set))
                (set a  c d ))
  )

(: keep-by-triplet-distance : (Listof Path) (Listof AgePathPair) -> (Setof Path))
;; keep only backups that are relevant for this interval set
(define (keep-by-triplet-distance _ backup-age-pairs)
  (define rev-sorted-age-pairs (reverse (sort-by-age backup-age-pairs)))
  (cond ((> 3 (length backup-age-pairs))
         (age-path-pairs->paths backup-age-pairs))
        (else
         (begin
           (define apip1 (first rev-sorted-age-pairs))
           (define api (second rev-sorted-age-pairs))
           (define apim1 (third rev-sorted-age-pairs))
           (-keep-by-triplet-distance (drop rev-sorted-age-pairs 3) apim1 api apip1 (set))))))

(module+ test #| keep-by-triplet-distance |#
  (check-equal? (keep-by-triplet-distance null '())
                (set))
  (check-equal? (keep-by-triplet-distance null `((1 ,a)))
                (set a))
  (check-equal? (keep-by-triplet-distance null `((1 ,a) (2 ,b)))
                (set a b))
  (check-equal? (keep-by-triplet-distance null `((1 ,a) (2 ,b) (3 ,c)))
                (set a b c))
  (check-equal? (keep-by-triplet-distance null `((5 ,a) (6 ,b) (7 ,c) (22 ,d)))
                (set a  c d )))

(: backup-keep-functions (Listof KeepFunction))
;; list of predicates to apply when selecting full backups to keep
(define backup-keep-functions
  (list (curry keep-first-n 4)
        keep-by-triplet-distance))

(module+ test #| keep paths |#
  (check-equal? (--kept-paths '() '() (set) '())
                (set))
  (check-equal? (--kept-paths `(,valid-path-20200101)
                             `((1 ,valid-path-20200101))
                             (set)
                             backup-keep-functions)
                (set valid-path-20200101))
  (check-equal? (--kept-paths `(,valid-path-20200101 ,valid-path-20200201 ,valid-path-20200203 ,valid-path-20200502, valid-path-20200514)
                             `((1 ,valid-path-20200101)
                               (2 ,valid-path-20200201)
                               (3 ,valid-path-20200203)
                               (4 ,valid-path-20200502)
                               (5 ,valid-path-20200514))
                             (set)
                             (list (curry keep-first-n 4)))
                (set valid-path-20200101 valid-path-20200201 valid-path-20200203 valid-path-20200502)))

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
                (set a b c d e g)) ;; dropped f
  (check-equal? (kept-paths (list a b c d f g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g)))
                (set a b c d e g))
  (check-equal? (kept-paths (list a b c d e g)
                            `((0 ,a) (1 ,a) (2 ,a) (3 ,b) (4 ,c) (5 ,c) (6 ,c) (7 ,d) (8 ,e) (9 ,f) (10 ,f) (11 ,f) (12 ,g) (13 ,g) (14 ,g) (15 ,h)))
                (set a b c d f h))
  (check-equal? (kept-paths (list a b c d e g h)
                            `((2 ,a) (3 ,b) (4 ,c)  (7 ,d) (8 ,e)  (9 ,f)  (12 ,g) (15 ,h)))
                (set a b c d e g h))
  (check-equal? (kept-paths (list l m n o p r u z)
                            `((0 ,l) (1 ,m) (2 ,n) (3 ,o) (4 ,p) (6 ,r) (9 ,u) (14 ,z)))
                (set l m n o p r u z)) ;; drop none
  (check-equal? (kept-paths (list k l m n o p r u z)
                            `((0 ,k) (1 ,l) (2 ,m) (3 ,n) (4 ,o) (5 ,p) (7 ,r) (10 ,u) (15 ,z)))
                (set k l m n o p r u z)) ;; drop none
  (check-equal? (kept-paths (list j k l m n p r u z)
                            `((0 ,j) (1 ,k) (2 ,l) (3 ,m) (4 ,n) (6 ,p) (8 ,r) (11 ,u) (16 ,z)))
                (set j k l m n p r u z)) ;; drop u
  (check-equal? (kept-paths (list i j k l m n r u z)
                            `((0 ,i) (1 ,j) (2 ,k) (3 ,l) (4 ,m) (5 ,n) (9 ,r) (12 ,u) (17 ,z)))
                (set i j k l m n r u z)) ;; drop none
  [check-equal? (kept-paths (list h i j k l m n r u z)
                            `((0 ,h) (1 ,i) (2 ,j) (3 ,k) (4 ,l) (5 ,m) (6 ,n) (10 ,r) (13 ,u) (18 ,z)))
                (set h i j k l n r u z)] ;; drop m
  (check-equal? (kept-paths (list g h i j k m n r u z)
                            `((0 ,g) (1 ,h) (2 ,i) (3 ,j) (4 ,k) (6 ,m) (7 ,n) (11 ,r) (14 ,u) (19 ,z)))
                (set g h i j k m n r u z)) ;; drop none
  (check-equal? (kept-paths (list f g h i j k m n r z)
                            `((0 ,f) (1 ,g) (2 ,h) (3 ,i) (4 ,j) (5 ,k) (7 ,m) (8 ,n) (12 ,r)(20 ,z)))
                (set f g h i j k m n r z)) ;; drop none
  (check-equal? (kept-paths (list e f g h i j k m n r z)
                            `((0 ,e) (1 ,f) (2 ,g) (3 ,h) (4 ,i) (5 ,j) (6 ,k) (8 ,m) (9 ,n) (13 ,r)(21 ,z)))
                (set e f g h i k n r z))  ;; drop j, m
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
        [else (--validated-intervals (drop sorted-ages 1) (set-add result (interval-num-of (first sorted-ages))))]))

(module+ test #| validated-intervals |#
  (check-equal? (validated-intervals '(0 1 2 3 4 5 6 7 8 9 10))
                (set 0 1 2 3 4 5))
  (check-equal? (validated-intervals '(0 1 2 3 5 8 13))
                (set 0 1 2 3 4 5 6))
  (check-equal? (validated-intervals '())
                (set))
  (check-equal? (validated-intervals '(0 10))
                (set 0 5))
  (check-equal? (validated-intervals '(0 4))
                (set 0 3))
  (check-equal? (validated-intervals '(0 3 4 8))
                (set 0 3 5))
  (check-equal? (validated-intervals '(0 3 5 7))
                (set 0 3 4))
  (check-equal? (validated-intervals '(0 3 5 7 13))
                (set 0 3 4 6))
  )

(: validated-intervals : (Listof Nonnegative-Integer) -> (Setof Nonnegative-Integer))
;; get the valid/satisfied intervalls by fib-numbers of the given backup ages, taking distances between backups into account
(define (validated-intervals sorted-ages)
  (--validated-intervals sorted-ages (set)))

(: get-validated-intervals (((Listof Path)) (Date) . ->* . (Setof Nonnegative-Integer)))
;; classify sig files of full backups into 'keep or 'discard
(define (get-validated-intervals sigfiles [reference-date (gg:now)])
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
                (set 1 2 3 4))
  (check-equal? (unsatisfied-intervals '(0 4))
                (set 1 2))
  (check-equal? (unsatisfied-intervals '(0 3 4 8))
                (set 1 2 4))
  (check-equal? (unsatisfied-intervals '(0 3 5 7))
                (set 1 2))
  (check-equal? (unsatisfied-intervals '(0 3 5 7 13))
                (set 1 2 5)))

(: unsatisfied-intervals : (Listof Nonnegative-Integer) -> (Setof Nonnegative-Integer))
;; return the intervals that are not satisfied by the given backup ages, taking distances between backups into account
(define (unsatisfied-intervals sorted-ages)
  (cond [(empty? sorted-ages)
         (set)]
        [else
         (define valid-intervals (validated-intervals sorted-ages))
         (define max-interval (apply max (set->list valid-intervals)))
         (set-subtract (list->set (range max-interval)) (validated-intervals sorted-ages))]))

(: --set-filter (All (A) (-> (Setof A) (Setof A) (-> A Boolean) (Setof A))))
;; filter the unfiltered set by the predicate, filling the filtered-set with elements that satisfy the predicate
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

(: file-sizes : (Listof Path) String -> Integer)
;; sum of the file sizes of the given list of file paths
(define (file-sizes files backup-dir)
  (foldl +  0 (map (lambda ([path : Path])
                     (when (path? path)
                       (define str (path->string (build-path backup-dir path)))
                       (file-size str))) files)))

(: bytes->string : Integer -> String)
;; transform number of bytes into human readable string
(define (bytes->string bytes)
  (cond [(<= bytes 1024) (format "~a Bytes" bytes)]
        [(<= bytes (expt 1024 2)) (format "≈ ~a kB" (arithmetic-shift bytes -10))]
        [(<= bytes (expt 1024 3)) (format "≈ ~a MB" (arithmetic-shift bytes -20))]
        [(<= bytes (expt 1024 4)) (format "≈ ~a GB" (arithmetic-shift bytes -30))]
        [(<= bytes (expt 1024 5)) (format "≈ ~a TB" (arithmetic-shift bytes -40))]
        [else "very large"]))

(: check-backups : String -> Void)
;; simple check and print of current kept and discarded backups (no actual actions taken)
(define (check-backups profile)
  (unless (cl-force)
    (log-msg 1 "dry-run\ndeleting nothing (use --force to actually delete if appropriate)"))
  (log-msg 5 "locating configuration")
  (define config     (read-configuration (file->lines (build-path (find-system-path 'home-dir) ".duplicity/config")) profile))
  (log-msg 2 (format "using configuration profile ~a" profile))
  (define backup-dir (hash-ref config 'backup-folder))
  (log-msg 1 (format "processing backup in ~a" backup-dir))
  (cond [(directory-exists? backup-dir)
         (log-msg 6 (format "dir ~s exists" backup-dir))
         (define full-backup-files   (directory-list backup-dir))
         (log-msg 6 "read full backup list")
         (define full-sig-files      (filter matched-backup-file full-backup-files))
         (log-msg 5 (format "sigfiles: ~a" full-sig-files))
         (log-msg 6 "read signature files")
         (define classified-sigfiles (classify-sigfiles full-sig-files))
         (log-msg 6 "classified signature files")
         (define sec-dump            (hash-ref config 'temp-folder))
         (define discarded-dep-files (map (lambda ([path : Path])
                                            (get-chains-related-to path full-backup-files))
                                          (set->list (hash-ref classified-sigfiles 'discard))))
         (define discarded-full-files (map (lambda ([path : Path])
                                             (get-full-related-to path full-backup-files))
                                           (set->list (hash-ref classified-sigfiles 'discard))))
         (log-msg 6 "collected complete list of files to discard")
         (define intervals           (get-validated-intervals full-sig-files))
         (log-msg 5 (format "calculated validated intervals before deletion"))
         (define intervals-after     (get-validated-intervals (set->list (hash-ref classified-sigfiles 'keep))))
         (log-msg 3 (format "validated interval(s) ~s" intervals))
         (log-msg 3 (format "validated interval(s) upon removal ~s" intervals-after))
         (log-msg 4 (format "keep ~s" (hash-ref classified-sigfiles 'keep)))
         (log-msg 3 (format "keeping ~a full backup(s)" (set-count (hash-ref classified-sigfiles 'keep))))
         (log-msg 4 (format "discard ~s" (hash-ref classified-sigfiles 'discard)))
         (log-msg 1 (format "discarding ~a full backup(s)" (set-count (hash-ref classified-sigfiles 'discard))))
         (define all-files-to-discard (append (flatten discarded-full-files) (flatten discarded-dep-files)))
         (log-msg 6 (format "discard along with sigfile, dependend files: ~a" (length all-files-to-discard)))
         (define size-to-discard (file-sizes (cast all-files-to-discard (Listof Path)) backup-dir))
         (log-msg 1 (format "discarding size ~a" (bytes->string size-to-discard)))
         (cond [(and (set=? intervals intervals-after)
                     (not (set-empty? (hash-ref classified-sigfiles 'discard))))
                (log-msg 1 "discarding superfluous backups ...")
                (for-each (lambda ([path : Any])
                            (when (path? path)
                              (begin
                                (log-msg 5 (format "moving discarded file ~s to ~s" (path->string path) sec-dump))
                                (when (and (log-level<= 4)
                                         (log-level>= 1))
                                  (write-rotating-bar))
                                (when (cl-force)
                                  (rename-file-or-directory (build-path backup-dir path) (build-path sec-dump path))))))
                          all-files-to-discard)]
               [(not (set=? intervals intervals-after))
                (log-msg 1 "WARNING: not doing anything, since interval would be incomplete.")]
               [else
                (log-msg 1 "nothing discarded")])]
        [else
         (log-msg 1 (format "dir ~s does not exist" backup-dir))]))

(: string->loglevel : String -> Nonnegative-Integer)
;; transform string to the log level number
(define (string->loglevel str)
  (define num (exact-floor (real-part (or (string->number str) 3))))
  (if (nonnegative-integer? num)
    num
    3))

(: log-level<= : Integer -> Boolean)
;; is the given log level is greater or equal the configured log level?
(define (log-level<= level)
  (<= (cl-verbosity) level))

(: log-level>= : Integer -> Boolean)
;; is the given log level less or equal to the configured log level?
(define (log-level>= level)
  (>= (cl-verbosity) level))

(: log-msg : Integer String -> Void)
;; log the given string if the level should be logged
(define (log-msg level message)
  (when (log-level>= level)
    (for-each (lambda (line)
                (when (cl-show-log-levels)
                  (with-colors (background-color) 'blue
                    (lambda () (printf (format "L~a: " level)))))
                (printf (format "~a\n" line)))
              (string-split message "\n"))))

(: rotating-bar-index Nonnegative-Integer)
(define rotating-bar-index 0)
(: rotating-bar-strings String)
(define rotating-bar-strings  ".oO0Oo") ;; "|/-\\"
(: rotating-bar-last-output Real)
(define rotating-bar-last-output #i1)
(define (write-rotating-bar)
  (define now (exact->inexact (gg:current-posix-seconds)))
  (when (<= 1 (- now rotating-bar-last-output))
    (printf (format "~a" (string-ref rotating-bar-strings rotating-bar-index)))
    (flush-output)
    (printf "\033[1D")
    (set! rotating-bar-last-output now)
    (set! rotating-bar-index (remainder (add1 rotating-bar-index) (string-length rotating-bar-strings)))))

(: cl-profile (Parameterof String))
(define cl-profile (make-parameter "default"))
(: cl-verbosity (Parameterof Nonnegative-Integer))
(define cl-verbosity (make-parameter 3 ))
(: cl-force (Parameterof Boolean))
(define cl-force (make-parameter #f))
(: cl-show-log-levels (Parameterof Boolean))
(define cl-show-log-levels (make-parameter #f))

(command-line
 #:usage-help
 "This will check backup interval coverage using fibonacci numbers, "
 "discarding old backups that do not add to interval coverage now, "
 "nor anytime in the future."
 ""
 "NOTHING IS CHANGED UNLESS OPTION '-f' or '--force' IS GIVEN"
 ""
 #:once-each
 [("-p" "--profile")
  profile "select a backup profile for configuration"
  (cl-profile (if (string? profile) profile "default"))]
 [("-l" "--show-log-level")
  "show log level before message"
  (cl-show-log-levels #t)]
 [("-v" "--verbose" "--verbosity")
  verbosity "number 0..9 of verbosity"
  (cl-verbosity (string->loglevel (if (string? verbosity) verbosity "3")))]
 [("-f" "--force")
  "force actual (re)moval of files"
  (cl-force #t)])

(check-backups (cl-profile))
(log-msg 1 "done")
