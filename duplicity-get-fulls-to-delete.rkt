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

(if (not (directory-exists? backup-dir))
    (printf "dir does not exist\n")
    (begin
      (printf "dir exists\n")
      (let* ([full-backup-files (directory-list backup-dir)]
             [full-sig-files    (filter matched-backup-file full-backup-files)]
             [full-backup-dates (map backup-date full-sig-files)])
        (for-each (lambda (arg) (printf "got ~s\n" (backup-age-in-months arg))) full-backup-dates))))
