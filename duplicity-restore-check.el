(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'async)
(require 'cl-seq)
(require 'pcre)
(require 'dired-async)

(defun duplicity--get-config-alist (file profile)
  "retrieve duplicity backup configuration for PROFILE from the given FILE

an alist is returned allowing access like (cdr (assoc \"encryption-key\" alist))"
  (let* ((config (slurp file))
         (config-lines (split-string config "\n"))
         (profile-start (--drop-while (not (string-prefix-p (format "[%s]" profile) it)) config-lines))
         (profile-lines (--take-while (not (string-prefix-p "[" it)) (-drop 1 profile-start)))
         (actual-config (--filter (not (string-prefix-p "#" it)) profile-lines))
         (actual-alist  (--map (when (string-match "^\\([a-zA-Z-_]*\\): \\([^#]*\\)" it)
                                 (cons (match-string 1 it) (match-string 2 it)))
                               actual-config)))
    actual-alist))

(defun md5sum (filename)
  "return nil or the md5sum as string of FILENAME"
  (when (file-regular-p (expand-file-name filename))
    (car
     (split-string
      (string-trim
       (shell-command-to-string
        (format "md5sum %s" (shell-quote-argument (expand-file-name filename)))))
      " "))))

(defun duplicity--restore-file (backup-location filename target-filename)
  "restore FILENAME in duplicity backup at BACKUP-LOCATION to TARGET-FILENAME"
  (shell-command
   (format "duplicity restore --use-agent --file-to-restore %s %s %s"
           (shell-quote-argument filename)
           (shell-quote-argument (concat "file://" backup-location))
           (shell-quote-argument (expand-file-name target-filename)))))

(defun duplicity--list-current-files-into (backup-location buffer-name)
  "restore FILENAME in duplicity backup at BACKUP-LOCATION to TARGET-FILENAME"
  (get-buffer-create buffer-name)
  (with-current-buffer buffer-name
    (delete-region (point-min) (point-max))
    (shell-command
     (format "duplicity list-current-files '%s'" (concat "file://" backup-location))
     (current-buffer))
    (goto-char (point-min))
    ;; kill first two lines containing meta data of this backup
    (kill-line)
    (kill-line)))

;; (duplicity--list-current-files-into  "/run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*")

(defun duplicity--files-buffer--get (backup-location buffer-name)
  "get backup list into buffer or reuse buffer if already there"
  (unless (get-buffer buffer-name)
    (duplicity--list-current-files-into backup-location buffer-name))
  (get-buffer buffer-name))

;; (get-buffer "*backup files*")
;; (duplicity--files-buffer--get  "/run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*")

(defun duplicity--files-buffer--get-number (backup-location buffer-name)
  "get the (total) number of files in the backup (including directories)"
  (with-current-buffer (duplicity--files-buffer--get backup-location buffer-name)
    (1- (car (buffer-line-statistics)))))

(defun duplicity--files-buffer--get-file-at (backup-location buffer-name idx)
  "get the filename at IDX (may be a directory)"
  (with-current-buffer (duplicity--files-buffer--get backup-location buffer-name)
    (goto-char (point-min))
    (forward-line (1- idx))
    (move-to-column 25)
    (buffer-substring-no-properties (point) (line-end-position))))

;; (duplicity--files-buffer--get-file-at  "/run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*" 48)

(defun duplicity--make-folder-prefix (folder-backed-up)
  (if (string-suffix-p "/" folder-backed-up)
      folder-backed-up
    (concat folder-backed-up "/")))

(defun duplicity--files-buffer--get-regular-file-from (backup-location buffer-name folder-backed-up idx threshold)
  "get the first regular file from the backup starting at IDX max IDX + THRESHOLD"
  (when (< 0 threshold)
    (let ((result (duplicity--files-buffer--get-file-at backup-location buffer-name idx)))
      (if (file-regular-p (expand-file-name (concat (duplicity--make-folder-prefix folder-backed-up) result)))
          result
        (duplicity--files-buffer--get-regular-file-from backup-location buffer-name folder-backed-up (1+ idx) (1- threshold))))))

;; (duplicity--files-buffer--get-regular-file-from  "file:///run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*" "~" 48 2)

(defun duplicity--files-buffer--static-probes-for-restore-check  (backup-location buffer-name)
  (with-current-buffer (duplicity--files-buffer--get backup-location buffer-name)
    (--filter it
              (-map (lambda (file-name)
                      (progn
                        (goto-char 0)
                        (if (pcre-re-search-forward (format "^.{24} %s$" (regexp-quote file-name)) nil t)
                          file-name
                          (progn
                            (message "skipping file '%s' during restore check (reason: not found in backup)" file-name)
                            nil))))
                    (if (boundp 'duplicity--static-file-list-for-restore-check)
                        duplicity--static-file-list-for-restore-check
                      '())))))

;; (duplicity--files-buffer--static-probes-for-restore-check "file:///run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*")

(defun duplicity--files-buffer--probes-for-restore-check (backup-location buffer-name folder-backed-up random-num)
  "provide a list of files for restore check.

this will include: 
  the 100th file, the (total-num - 100)th file
  log(total-num) number of files spread over the backup
  RANDOM-NUM number of files, randomly picked"
  (with-current-buffer (duplicity--files-buffer--get backup-location buffer-name)
    (let ((total-num (duplicity--files-buffer--get-number backup-location buffer-name))
          (regular-file-lookahead-threshold 10))
      (--filter it
                (append
                 (when (> total-num 300)
                   (list (duplicity--files-buffer--get-regular-file-from backup-location buffer-name folder-backed-up 100 regular-file-lookahead-threshold)
                         (duplicity--files-buffer--get-regular-file-from backup-location buffer-name folder-backed-up (- total-num 100) regular-file-lookahead-threshold)))       
                 (--map (duplicity--files-buffer--get-regular-file-from backup-location buffer-name folder-backed-up it regular-file-lookahead-threshold)
                        (gen-probe-numbers total-num))
                 (when (and (>= random-num 1)
                          (> total-num 100))
                   (--map (duplicity--files-buffer--get-regular-file-from backup-location buffer-name folder-backed-up it regular-file-lookahead-threshold)
                          (--map (1+ (cl-random (- total-num regular-file-lookahead-threshold)))
                                 (number-sequence 1 random-num)))))))))

(defun gen-probe-numbers (total-num)
  "get a list of log(TOTAL-NUM) numbers, spread evenly in 1..TOTAL-NUM"
  (let* ((reg-probes (floor (log total-num))))
    (--map (floor (* it (floor (/ total-num (1+ reg-probes)))))
           (number-sequence 1 reg-probes))))

;; (duplicity--files-buffer--probes-for-restore-check "file:///run/media/pe/684110cc-325f-4307-bce0-843930ff7de6/data-backup" "*backup files*" "~" 3)

(defun duplicity--readable-file-size (filename folder-backed-up)
  (file-size-human-readable
   (nth 7 (file-attributes (expand-file-name (concat (duplicity--make-folder-prefix folder-backed-up) filename))))))

;; (duplicity--readable-file-size ".bash_history" "~")

(defun duplicity--restore-file-and-compare (backup-folder filename folder-backed-up)
  "restore the FILENAME into a temporary file, return the hash md5 hashes match"
  (let ((temp-file (make-temp-file "restored"))
        (result nil)
        (rest-md5 nil))
    (delete-file temp-file)
    (duplicity--restore-file backup-folder filename temp-file)
    (ignore-errors
      (let ((orig-md5 (md5sum (expand-file-name (concat (duplicity--make-folder-prefix folder-backed-up) filename)))))
        (setq rest-md5 (md5sum temp-file))
        (with-temp-message (format "%s -> %s (%s): \"%s\"" orig-md5 rest-md5 (duplicity--readable-file-size filename folder-backed-up) filename))
        (setq result (and (string= orig-md5 rest-md5)
                        (file-readable-p temp-file)
                        (file-readable-p (expand-file-name (concat (duplicity--make-folder-prefix folder-backed-up) filename)))
                        (string-match-p "^[a-f0-9]*$" rest-md5)
                        (= 32 (length rest-md5))))
        (unless result (with-temp-message "FAILED COMPARE"))))
    (delete-file temp-file)
    (when result
        rest-md5)))

;; (string-match-p "^[a-f0-9]*$" "d41d8cd98f00b204e9800998ecf8427e")
;; (length "d41d8cd98f00b204e9800998ecf8427e")

(defun list-has-no-duplicates (values)
  (equal values (remove-duplicates values :test #'equal)))

(defun duplicity--check-latest-backup ()
  "check latest backup on some probes, reporting status and progress"
  (interactive)
  (let* ((config-alist (duplicity--get-config-alist "~/.duplicity/config" "default"))
         (key (cdr (assoc "encryption-key" config-alist)))
         (backup-folder (cdr (assoc "backup-folder" config-alist)))
         (folder-backed-up (cdr (assoc "folder" config-alist)))
         (backup-files-buffer "*backup files*"))
    (cond
     ((gb/backup-is-running)
      (message "backup currently running, please try later"))
     ((not (file-directory-p backup-folder))
      (message "backup folder %s not found" backup-folder))
     ((not (gb/gpg-key-query-if-locked key 3))
      (message "unlock the backup key first"))
     ((progn (message "collecting backup information ...")
             (< (duplicity--files-buffer--get-number backup-folder backup-files-buffer)
                500))
      (message "check makes only sense for backups of size 500+ files, found only %d"
               (duplicity--files-buffer--get-number backup-folder backup-files-buffer)))
     (t (duplicity--execute-backup-check backup-folder backup-files-buffer folder-backed-up)))))

(defun duplicity--check-add-modeline-status () "" (duplicity--async-modeline-mode 1) (setq duplicity--progress-string "...") (force-mode-line-update t))
(defun duplicity--check-update-modeline-status (new-str)  "" (setq duplicity--progress-string new-str) (force-mode-line-update t))
(defun duplicity--check-remove-modeline-status () "" (duplicity--async-modeline-mode 0) (setq duplicity--progress-string "...") (force-mode-line-update t))

(defvar duplicity--progress-string "..." "compare progress as string e.g. 5/10")

(define-minor-mode duplicity--async-modeline-mode
    "Notify mode-line that an async process run."
  :global t
  :lighter (:eval (propertize (format  " [compare running (%s)]" duplicity--progress-string)
                              'face 'dired-async-mode-message))
  (unless duplicity--async-modeline-mode
    (let ((visible-bell t)) (ding))))

(defun duplicity--execute-backup-check (backup-folder backup-files-buffer folder-backed-up)
  ""
  (unwind-protect
      (progn
        (duplicity--check-add-modeline-status)
        (let* ((num 1)
               (random-file-list (duplicity--files-buffer--probes-for-restore-check backup-folder backup-files-buffer folder-backed-up 4))
               (static-file-list (duplicity--files-buffer--static-probes-for-restore-check backup-folder backup-files-buffer))
               (file-list (append static-file-list random-file-list))
               (failed-file-list '())
               (last-num (length file-list))
               (collected-md5-hashes
                (-map (lambda (file-name)
                        (message "checking latest backup (%d/%d, failed %d: %s)" num last-num (length failed-file-list) file-name)                         
                        (duplicity--check-update-modeline-status (format "%d/%d" num last-num))
                        (setq num (1+ num))                 
                        (let ((result (duplicity--restore-file-and-compare backup-folder file-name folder-backed-up)))
                          (unless result
                            (setq failed-file-list (cons file-name failed-file-list)))
                          result))
                      file-list)))
          (if (and (null failed-file-list)
                 (> last-num 2)
                 (list-has-no-duplicates collected-md5-hashes))
              (message "backup extraction and compare successful")
            (read-answer (format "compare FAILED for files: %s, " (string-join failed-file-list ", "))
                         '(("accept" ?a "accept the difference and take measures"))))))
    (duplicity--check-remove-modeline-status)))

(defun duplicity--execute-backup-check-async (backup-folder backup-files-buffer folder-backed-up)
  "do actual backup check asynchronously"
  (duplicity--check-add-modeline-status)
  (let* ((num 1)
         (random-file-list (duplicity--files-buffer--probes-for-restore-check backup-folder backup-files-buffer folder-backed-up 4))
         (static-file-list (duplicity--files-buffer--static-probes-for-restore-check backup-folder backup-files-buffer))
         (file-list (append static-file-list random-file-list))
         (reporter (make-progress-reporter "comparing ..." )))
    ;; (dired-async-mode-line-message "starting compare" 'dired-async-mode-message)
    (async-start `(lambda ()
                    ,(async-inject-environment "load-path")
                    (require 'duplicity-restore-check)                
                    (let ((num 1)
                          (last-num (length (quote ,file-list))))
                      (cl-maplist (lambda (file-name)
                                    (progn
                                      (async-send :status 
                                                  (format "checking latest backup (%d/%d: %s)" num last-num (car file-name)))                                            
                                      (setq num (1+ num))
                                      (cons (car file-name) (duplicity--restore-file-and-compare ,backup-folder (car file-name) ,folder-backed-up))))
                                  (quote ,file-list))))
                 `(lambda (collected-fn-hash-pairs)
                    (require 'dired-async)
                    (if (async-message-p collected-fn-hash-pairs)
                        (progn
                          (duplicity--check-update-modeline-status (plist-get collected-fn-hash-pairs ':status))
                          (progress-reporter-force-update (quote ,reporter))
                          (message (plist-get collected-fn-hash-pairs ':status)))
                      (let ((collected-md5-hashes (cl-remove-if 'null (cl-maplist 'cdar collected-fn-hash-pairs)))
                            (failed-file-list (cl-maplist 'caar (cl-remove-if-not '(lambda (lipair) (null (cdr lipair))) collected-fn-hash-pairs))))
                        (duplicity--check-remove-modeline-status)
                        (progress-reporter-done (quote ,reporter))
                        (if (and (null failed-file-list)
                               (> (length collected-fn-hash-pairs) 2)
                               (list-has-no-duplicates collected-md5-hashes))
                            (dired-async-mode-line-message "backup compare successful" 'dired-async-mode-message)
                          (dired-async-mode-line-message (format "compare FAILED for files: %s, " (string-join  failed-file-list ", ")) 'dired-async-mode-message))))))))

;; experiment to execute tests asynchronously
(defun duplicity--check-latest-backup-async ()
  (interactive)
  (let* ((config-alist (duplicity--get-config-alist "~/.duplicity/config" "default"))
         (key (cdr (assoc "encryption-key" config-alist)))
         (backup-folder (cdr (assoc "backup-folder" config-alist)))
         (folder-backed-up (cdr (assoc "folder" config-alist)))
         (backup-files-buffer "*backup files*"))
    (cond
     ((gb/backup-is-running)
      (message "backup currently running, please try later"))
     ((not (f-directory? backup-folder))
      (message "backup folder %s not found" backup-folder))
     ((not (gb/gpg-key-query-if-locked key 3))
      (message "unlock the backup key first"))
     ((progn (message "collecting backup information ...")
             (< (duplicity--files-buffer--get-number backup-folder backup-files-buffer)
                500))
      (message "check makes only sense for backups of size 500+ files, found only %d"
               (duplicity--files-buffer--get-number backup-folder backup-files-buffer)))
     (t (duplicity--execute-backup-check-async backup-folder backup-files-buffer folder-backed-up)))))

(provide 'duplicity-restore-check)
