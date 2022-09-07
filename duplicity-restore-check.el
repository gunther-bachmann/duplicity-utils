(defun duplicity--get-config-alist (file profile)
  "retrieve duplicity backup configuration for PROFILE from the given FILE

an alist is returned allowing access like (cdr (assoc \"encryption-key\" alist))"
  (let* ((config (slurp file))
         (config-lines (split-string config "\n"))
         (profile-start (--drop-while (not (s-starts-with? (format "[%s]" profile) it)) config-lines))
         (profile-lines (--take-while (not (s-starts-with? "[" it)) (-drop 1 profile-start)))
         (actual-config (--filter (not (s-starts-with? "#" it)) profile-lines))
         (actual-alist  (--map (when (string-match "^\\([a-zA-Z-_]*\\): \\([^#]*\\)" it)
                                 (cons (match-string 1 it) (match-string 2 it)))
                               actual-config)))
    actual-alist))

(defun md5sum (filename)
  "return nil or the md5sum as string of FILENAME"
  (when (file-regular-p (expand-file-name filename))
    (first
     (split-string
      (string-trim
       (shell-command-to-string
        (format "md5sum '%s'" (expand-file-name filename))))
      " "))))

(defun duplicity--restore-file (backup-location filename target-filename)
  "restore FILENAME in duplicity backup at BACKUP-LOCATION to TARGET-FILENAME"
  (shell-command
   (format "duplicity restore --use-agent --file-to-restore '%s' '%s' '%s'"
           filename (concat "file://" backup-location) (expand-file-name target-filename))))

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
                        (= 32 (length rest-md5))))))
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
     ((not (f-directory? backup-folder))
      (message "backup folder %s not found" backup-folder))
     ((not (gb/gpg-key-query-if-locked key 3))
      (message "unlock the backup key first"))
     ((progn (message "collecting backup information ...")
             (< (duplicity--files-buffer--get-number backup-folder backup-files-buffer)
                500))
      (message "check makes only sense for backups of size 500+ files, found only %d"
               (duplicity--files-buffer--get-number backup-folder backup-files-buffer)))
     (t (let* ((success t)
               (num 1)
               (file-list (duplicity--files-buffer--probes-for-restore-check backup-folder backup-files-buffer folder-backed-up 4))
               (last-num (length file-list))
               (collected-md5-hashes
                (-map-when (lambda (file-name) success)
                           (lambda (file-name)
                             (message "checking latest backup (%d/%d: %s)" num last-num file-name)                         
                             (setq num (1+ num))
                             (setq success (duplicity--restore-file-and-compare backup-folder file-name folder-backed-up)))
                           file-list)))
          (setq success (and success
                           (> last-num 2)
                           (list-has-no-duplicates collected-md5-hashes)))
          (if success
              (message "backup extraction and compare successful")
            (read-answer "backup extraction and compare FAILED"
                         '(("accept" ?a "accept the difference and take measures")))))))))

;; (duplicity--check-latest-backup)








