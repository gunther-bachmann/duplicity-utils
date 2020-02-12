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
  (check-equal? (discarded-paths '(a b c d e f g)
                                 '((0 a) (1 b) (2 c) (3 d) (4 e) (5 f) (6 g)))
                (set 'e)))

(define (discarded-paths all-paths age-path-pairs)
  (let* ([filled-age-file-pairs       (fill-gaps age-path-pairs)]
         [fib-filtered-age-file-pairs (filter keep-backup-since-age-is-fib? filled-age-file-pairs)]
         [fib-kept-paths              (age-path-pairs->paths fib-filtered-age-file-pairs)]
         [oldest-path                 (set (cadr (last age-path-pairs)))]
         [four-youngest               (age-path-pairs->paths (take age-path-pairs 4))]
         [all-kept                    (set-union fib-kept-paths oldest-path four-youngest)]
         [discard                     (set-subtract (list->set all-paths) all-kept)])
    discard))

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

(if (not (directory-exists? backup-dir))
    (printf "dir does not exist\n")
    (begin
      (printf "dir exists\n")
      (let* ([full-backup-files           (directory-list backup-dir)]
             [full-sig-files              (filter matched-backup-file full-backup-files)]
             [age-file-pairs              (sort-by-age (pair-with-age full-sig-files))]
             [filled-age-file-pairs       (fill-gaps age-file-pairs)]
             [fib-filtered-age-file-pairs (filter keep-backup-since-age-is-fib? filled-age-file-pairs)]
             [fib-kept-paths              (age-path-pairs->paths fib-filtered-age-file-pairs)]
             [oldest-path                 (set (cadr (last age-file-pairs)))]
             [four-youngest               (age-path-pairs->paths (take 4 age-file-pairs))]
             [all-kept                    (set-union fib-kept-paths oldest-path four-youngest)]
             [discard                     (set-subtract (list->set full-sig-files) all-kept)])
        (for-each (lambda (arg) (printf "age: ~s, path: ~s\n" (car arg) (path->string (cadr arg)))) age-file-pairs)
        (print all-kept)
        (print discard))))
