#! /usr/bin/env racket
#lang racket

;; problem is that no racket packages can be installed into the racket-minimal installation used by shell-nix
;; #! /usr/bin/env nix-shell
;; #! nix-shell -i racket -p racket-minimal

;; raco pkg install gregor
(require gregor)
(require gregor/period)

(define backup-dir "/run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup")
(define full-backup-ls-pattern "duplicity-full-signatures\\..*\\.sigtar\\.gpg$")

(module+ test
  (require rackunit)
  (define valid-path-20200101
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200101T182223Z.sigtar.gpg"))
  (define valid-path-20200201
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (define valid-path-20200203
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200203T112223Z.sigtar.gpg"))
  (define valid-path-20200514
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200514T082223Z.sigtar.gpg"))
  (define valid-path-20200502
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signatures.20200502T092223Z.sigtar.gpg"))
  (define invalid-path
    (string->path "/run/media/pe/harddrive/data-backup/duplicity-full-signures.20200201T172223Z.sigtar.gpg")))

(define (matched-backup-file path)
  (regexp-match full-backup-ls-pattern (path->string path) ))

(module+ test
  (check-equal? (matched-backup-file valid-path-20200201)
                '("duplicity-full-signatures.20200201T172223Z.sigtar.gpg"))
  (check-false (matched-backup-file invalid-path)))

(define (backup-date path)
  (parse-date (regexp-replace ".*signatures\\.(.*)\\.sigtar.gpg" (path->string path) "\\1") "yyyyMMdd'T'HHmmssX"))

(module+ test
  (check date=?
         (backup-date valid-path-20200201)
         (date 2020 02 01))
  (check-exn exn:fail?
             (lambda () (backup-date invalid-path))))

(define (backup-age-in-months path [reference (now)])
  (period-ref (period-between (backup-date path) reference '(months)) 'months))

(module+ test
  (check-equal? (backup-age-in-months valid-path-20200201 (datetime 2020 07 01))
                5)
  (check-equal? (backup-age-in-months valid-path-20200203 (datetime 2020 07 01))
                4)
  (check-equal? (backup-age-in-months valid-path-20200514 (datetime 2020 07 01))
                1)
  (check-equal? (backup-age-in-months valid-path-20200502 (datetime 2020 07 01))
                1))

;; (printf "Given arguments: ~s\n"
;;         (current-command-line-arguments))

(define (pair-with-age paths [reference-date (now)])
  (map (lambda (path) `(,(backup-age-in-months path reference-date) ,path)) paths))

(module+ test
  (check-equal? (pair-with-age (list valid-path-20200201 valid-path-20200203) (datetime 2020 07 01))
                `((5 ,valid-path-20200201) (4 ,valid-path-20200203))))

(define (sort-by-age age-path-pairs)
  (sort age-path-pairs < #:key (lambda (pair) (car pair))))

(module+ test
    (check-equal? (sort-by-age `((5 ,valid-path-20200201) (4 ,valid-path-20200203)))
                `((4 ,valid-path-20200203) (5 ,valid-path-20200201) )))

(define (--fill-gaps rest-sorted-age-path-pairs n result)
  (cond [(empty? rest-sorted-age-path-pairs)
         result]
        [(and (= 1 (length rest-sorted-age-path-pairs))
              (> n (caar rest-sorted-age-path-pairs)))
         result]
        [(= 1 (length rest-sorted-age-path-pairs))
         (--fill-gaps rest-sorted-age-path-pairs
                     (+ 1 n)
                     (cons `(,n ,(cadar rest-sorted-age-path-pairs)) result))]
        [(< n  (caadr rest-sorted-age-path-pairs))
         (--fill-gaps rest-sorted-age-path-pairs
                     (+ 1 n)
                     (cons `(,n ,(cadar rest-sorted-age-path-pairs)) result))]
        [(>= n  (caadr rest-sorted-age-path-pairs))
         (--fill-gaps (cdr rest-sorted-age-path-pairs)
                     (+ 1 n)
                     (cons `(,n ,(cadadr rest-sorted-age-path-pairs)) result))]
        [#t result]))

(module+ test
  (check-equal? (--fill-gaps '((2 a)(3 b)(7 c)) 0 '())
                '((7 c) (6 b) (5 b) (4 b) (3 b) (2 a) (1 a) (0 a)))
  (check-equal? (--fill-gaps '((0 a)(2 b)(7 c)) 0 '())
                '((7 c) (6 b) (5 b) (4 b) (3 b) (2 b) (1 a) (0 a)))
  (check-equal? (--fill-gaps '((1 a)(5 b)(7 c)) 0 '())
                '((7 c) (6 b) (5 b) (4 a) (3 a) (2 a) (1 a) (0 a)))
  (check-equal? (--fill-gaps '((2 a)(5 b)) 0 '())
                '( (5 b) (4 a) (3 a) (2 a) (1 a) (0 a)))
  (check-equal? (--fill-gaps '((2 a)) 0 '())
                '(  (2 a) (1 a) (0 a)))
  (check-equal? (--fill-gaps '() 0 '())
                '()))

(define (fill-gaps sorted-age-path-pairs)
  (reverse (--fill-gaps sorted-age-path-pairs 0 '())))

(define (fib n)
  (cond [(= n 0)  0]
        [(<= n 2) 1]
        [#t       (+ (fib (- n 1)) (fib (- n 2)))]))

(define fib-backup-ages-to-keep (list->set (map fib (stream->list (in-range 0 15)))))

(module+ test
  (check-true (set-member? fib-backup-ages-to-keep (fib 10)))
  (check-true (set-member? fib-backup-ages-to-keep 0))
  (check-false (set-member? fib-backup-ages-to-keep (- (fib 10) 1))))

(define (keep-backup-since-age-is-fib? age-backup-pair)
  (set-member? fib-backup-ages-to-keep (car age-backup-pair)))

(define (age-path-pairs->paths age-path-pairs)
  (list->set (flatten (map cdr age-path-pairs))))

(module+ test
  (check-equal? (age-path-pairs->paths '((0 a) (1 a) (2 b) (3 b) (4 c)))
                (list->set '(a b c))))

(module+ test
  (check-equal? (kept-paths '(a b c d e f g)
                            '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f) (6 g)))
                (list->set '(a b c d f g))) ;; dropped e
  (check-equal? (kept-paths '(a b c d f g)
                            '((0 a) (1 a) (2 a) (3 b) (4 c) (5 c) (6 c) (7 d) (8 e) (9 f) (10 f) (11 f) (12 g)))
                (list->set '(a b c d e g)))
  (check-equal? (kept-paths '(a b c d e g)
                            '((0 a) (1 a) (2 a) (3 b) (4 c) (5 c) (6 c) (7 d) (8 e) (9 f) (10 f) (11 f) (12 g) (13 g) (14 g) (15 h)))
                (list->set '(a b c d e g h)))
  (check-equal? (kept-paths '(a b c d e f g)
                            (fill-gaps '((2 a) (3 b) (4 c)  (7 d) (8 e)  (9 f)  (12 g) (15 h))))
                (list->set '(a b c d e g h)))

  (check-equal? (kept-paths '(l m n o p r u z)
                            (fill-gaps '((0 l) (1 m) (2 n)  (3 o) (4 p)  (6 r)  (9 u) (14 z))))
                (list->set '(l m n o p r u z))) ;; drop none
  (check-equal? (kept-paths '(k l m n o p r u z)
                            (fill-gaps '((0 k) (1 l) (2 m) (3 n)  (4 o) (5 p)  (7 r)  (10 u) (15 z))))
                (list->set '(k l m n p r u z))) ;; drop o
  (check-equal? (kept-paths '(j k l m n p r u z)
                            (fill-gaps '((0 j) (1 k) (2 l) (3 m) (4 n) (6 p)  (8 r)  (11 u) (16 z))))
                (list->set '(j k l m n r u z))) ;; drop p
  (check-equal? (kept-paths '(i j k l m n r u z)
                            (fill-gaps '((0 i) (1 j) (2 k) (3 l) (4 m) (5 n) (9 r)  (12 u) (17 z))))
                (list->set '(i j k l m n r u z))) ;; drop none
  (check-equal? (kept-paths '(h i j k l m n r u z)
                            (fill-gaps '((0 h) (1 i) (2 j) (3 k) (4 l) (5 m) (6 n) (10 r)  (13 u) (18 z))))
                (list->set '(h i j k m n r u z))) ;; drop l
  (check-equal? (kept-paths '(g h i j k m n r u z)
                            (fill-gaps '((0 g) (1 h) (2 i) (3 j) (4 k) (6 m) (7 n) (11 r)  (14 u) (19 z))))
                (list->set '(g h i j k m n r z))) ;; drop u
  (check-equal? (kept-paths '(f g h i j k m n r z)
                            (fill-gaps '((0 f) (1 g) (2 h) (3 i) (4 j) (5 k) (7 m) (8 n) (12 r)(20 z))))
                (list->set '(f g h i j k m n r z))) ;; drop none
  (check-equal? (kept-paths '(e f g h i j k m n r z)
                            (fill-gaps '((0 e) (1 f) (2 g) (3 h) (4 i) (5 j) (6 k) (8 m) (9 n) (13 r)(21 z))))
                (list->set '(e f g h j m r z)))) ;; drop i, k, n

(define (--keep-because-it-becomes-fib sorted-age-path-pairs n kept-paths fib-distance)
  (cond [(= n 0) kept-paths]
        [#t (let* ([age-path-pair (list-ref sorted-age-path-pairs n)]
                   [age           (car age-path-pair)])
              (if (set-member? fib-backup-ages-to-keep (+ age fib-distance))
                  (--keep-because-it-becomes-fib sorted-age-path-pairs (- n 1) (cons (cadr age-path-pair) kept-paths) fib-distance)
                  (--keep-because-it-becomes-fib sorted-age-path-pairs (- n 1) kept-paths fib-distance)))]))

(module+ test
  (check-equal? (--keep-because-it-becomes-fib '((1 a) (5 b) (7 c) (15 d)) 3 '() 6)
                '(c d)))

(define (keep-because-it-becomes-fib sorted-age-path-pairs)
  (let* ([oldest-age (car (last sorted-age-path-pairs))]
         [min-fib-older-than-oldest (first (sort (filter (lambda (fnum) (>= fnum oldest-age)) (set->list fib-backup-ages-to-keep)) <))])
    (list->set (--keep-because-it-becomes-fib sorted-age-path-pairs (- (length sorted-age-path-pairs) 1) '() (- min-fib-older-than-oldest oldest-age)))))

(module+ test
  (check-equal? (keep-because-it-becomes-fib '((1 a) (5 b) (7 c) (15 d)))
                (set 'c 'd)))

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
  (if (not (directory-exists? backup-dir))
      (printf "dir does not exist\n")
      (begin
        (printf "dir exists\n")
        (let* ([full-backup-files  (directory-list backup-dir)]
               [full-sig-files     (filter matched-backup-file full-backup-files)]
               [age-file-pairs     (sort-by-age (pair-with-age full-sig-files))]
               [all-kept           (kept-paths full-sig-files age-file-pairs)]
               [discard            (set-subtract (list->set full-sig-files) all-kept)])
          (for-each (lambda (arg) (printf "age: ~s, path: ~s\n" (car arg) (path->string (cadr arg)))) age-file-pairs)
          (print all-kept)
          (print discard)))))
