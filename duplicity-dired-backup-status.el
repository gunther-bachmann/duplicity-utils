;; format of exclusion file
;; each line is:
;;   a folder name that is excluded (and all its subfolders and files, too)
;;   a file name that is excluded
;;   prefix '+ ' followed by a folder name, explicitly including the folder (and its subfolders and files)
;;   prefix '+ ' followed by a file name, explicitly including the file
;;
;; prefix '+ ' entries must follow after the excluding entry to be taken into account


;; TODO give functions consistent names
;; TODO find keybinding for marking all files

(eval-when-compile
  (require 'cl-lib))

(require 'dash)
(require 'ert)

(defconst duplicity-mount gb/backup-mount)

(defun duplicity--normalize-fs-name-for-prefix-check (fs-name)
  "ensure folders to end on '/' and files not to"
  (if (file-exists-p fs-name)
      (cond ((file-directory-p fs-name)
             (if (string-suffix-p "/" fs-name)
                 fs-name
               (format "%s/" fs-name)))
            (t fs-name))
    fs-name))

(defun duplicity--covered-by-backup-on-rule-line (covered-by-backup file-name-norm)
  "check the next covered-by-backup based on the passed COVERED-BY_BACKUP,
the given FILE-NAME-NORM alized and the current position in the exclude-file"
  (let ((lmin (point)))
    (end-of-line)
    (let* ((lmax (point))
           (line-str (buffer-substring-no-properties lmin lmax))
           (plus (string-prefix-p "+ " line-str))
           (line-str-norm (duplicity--normalize-fs-name-for-prefix-check
                           (if plus (substring line-str 2) line-str)))
           (matching-plus-rule
            (and plus
               (string-prefix-p line-str-norm file-name-norm)))
           (file-is-prefix-to-longer-rule
            (and (string-prefix-p file-name-norm line-str-norm)
               (not (eq (length file-name-norm) (length line-str-norm)))))
           (file-is-folder (string-suffix-p "/" file-name-norm)))
      (cond (matching-plus-rule 
             t)
            ((and (not (eq 'partial covered-by-backup))
                (or plus file-is-folder)
                file-is-prefix-to-longer-rule)
             'partial)
            ((eq t covered-by-backup)
             (not (string-prefix-p line-str-norm file-name-norm)))                                
            (t covered-by-backup)))))

(defun duplicity--map-files-covered-by-backup (exclusion-config file-name-list)
  "map each file/folder of the FILE-NAME-LIST to a pair
of backup coverage status (t nil partial) and the name
based on the given EXCLUSION-CONFIG"
  (with-temp-buffer
    (insert-file exclusion-config)    
    (cl-mapcar
     (lambda (file-name-el)  
       (cons
        (let ((file-name-norm (duplicity--normalize-fs-name-for-prefix-check file-name-el)))          
          (goto-char (point-min))
          (let ((covered-by-backup t))
            (while (not (eobp))
              (setq covered-by-backup
                    (duplicity--covered-by-backup-on-rule-line covered-by-backup file-name-norm))
              (forward-line 1)
              (beginning-of-line))
            covered-by-backup))
        file-name-el))
     file-name-list)))


;; partials

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--parent-folder ()
    "adding a folder which is a subfolder of the current folder being checked marks this as partial"
    (should (eq 'partial (with-temp-buffer
                           (insert "+ /some/folder/and/more/\n")
                           (goto-char (point-min))
                           (duplicity--covered-by-backup-on-rule-line nil "/some/folder/")))))

(ert-deftest duplicity--check-file-backup-status--plus-file-rule--parent-folder ()
    "adding a file which is in the current folder being checked marks this as partial"
    (should (eq 'partial (with-temp-buffer
                           (insert "+ /some/folder/file\n")
                           (goto-char (point-min))
                           (duplicity--covered-by-backup-on-rule-line nil "/some/folder/")))))

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--parent-folder-file ()
  "adding a folder which is a subfolder of a folder the current file being checked is in, is still excluded"
  (should-not (with-temp-buffer
                (insert "+ /some/folder/and/more/\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line nil "/some/folder/file"))))

;; regular exclusions

(ert-deftest duplicity--check-file-backup-status--folder-rule--sub-file ()
  "folder will ensure that direct sub files are excluded from backup"
  (should-not (with-temp-buffer
                (insert "/some/folder\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line t "/some/folder/sub-file"))))

(ert-deftest duplicity--check-file-backup-status--folder-rule--deep-sub-file ()
  "folder will ensure that deep sub files are excluded in backup"
  (should-not (with-temp-buffer
                (insert "/some/folder\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line t "/some/folder/other/folder/sub-file"))))

(ert-deftest duplicity--check-file-backup-status--folder-rule--sub-folder ()
  "folder will ensure that direct sub folder are excluded in backup"
  (should-not (with-temp-buffer
                (insert "/some/folder\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line t "/some/folder/sub-folder/"))))

(ert-deftest duplicity--check-file-backup-status--folder-rule--folder ()
  "folder will ensure that this folder is excluded in backup"
  (should-not (with-temp-buffer
                (insert "/some/folder\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line t "/some/folder/"))))

(ert-deftest duplicity--check-file-backup-status--folder-rule--deep-sub-folder ()
  "folder will ensure that deep sub folder are excluded in backup"
  (should-not (with-temp-buffer
                (insert "/some/folder\n")
                (goto-char (point-min))
                (duplicity--covered-by-backup-on-rule-line t "/some/folder/sub-folder/and/other/"))))

;; plus rules

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--sub-file ()
    "adding a folder will ensure that direct sub files are included in backup"
  (should (with-temp-buffer
            (insert "+ /some/folder\n")
            (goto-char (point-min))
            (duplicity--covered-by-backup-on-rule-line nil "/some/folder/sub-file"))))

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--deep-sub-file ()
    "adding a folder will ensure that deep sub files are included in backup"
  (should (with-temp-buffer
            (insert "+ /some/folder\n")
            (goto-char (point-min))
            (duplicity--covered-by-backup-on-rule-line nil "/some/folder/other/folder/sub-file"))))

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--sub-folder ()
    "adding a folder will ensure that direct sub folder are included in backup"
  (should (with-temp-buffer
            (insert "+ /some/folder\n")
            (goto-char (point-min))
            (duplicity--covered-by-backup-on-rule-line nil "/some/folder/sub-folder/"))))

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--folder ()
    "adding a folder will ensure that this folder is included in backup"
  (should (with-temp-buffer
            (insert "+ /some/folder\n")
            (goto-char (point-min))
            (duplicity--covered-by-backup-on-rule-line nil "/some/folder/"))))

(ert-deftest duplicity--check-file-backup-status--plus-folder-rule--deep-sub-folder ()
    "adding a folder will ensure that deep sub folder are included in backup"
  (should (with-temp-buffer
            (insert "+ /some/folder\n")
            (goto-char (point-min))
            (duplicity--covered-by-backup-on-rule-line nil "/some/folder/sub-folder/and/other/"))))

(defun duplicity--check-current-file-backup-status ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (ignore-errors
      (let ((file-name (duplicity--get-file-name-at-point)))
        (when (or (file-exists-p file-name) (file-directory-p file-name))
          (when-let ((found (cl-find-if
                             (lambda (pair) (string-equal
                                        (duplicity--normalize-fs-name-for-prefix-check file-name)
                                        (cdr pair)))
                             (duplicity--map-files-covered-by-backup
                              "~/.duplicity/data.exclude.list.txt"
                              (list (duplicity--normalize-fs-name-for-prefix-check file-name))))))
            (cond ((eq t (car found))
                   (pulsar-pulse-line)
                   (message "backed up"))
                  ((eq 'partial (car found))
                   (pulsar-pulse-line)
                   (message "partially backed up"))
                  (t (pulsar-highlight-line)
                     (message "not backed up")))))))))

(bind-key "C-c b i" #'duplicity--check-current-file-backup-status dired-mode-map)

(defun gb/get-list-of-files-in-current-dired-mode ()
  "collect all normalized full path file names of the current dired buffer"
  (when (eq major-mode 'dired-mode)
    (let ((file-name-list '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (end-of-line)
          (ignore-errors
            (let ((file-name (duplicity--get-file-name-at-point)))
              (when (or (file-exists-p file-name) (file-directory-p file-name))
                (setq file-name-list (cons (duplicity--normalize-fs-name-for-prefix-check file-name) file-name-list)))))
          (forward-line)
          ))
      file-name-list)))

(defun gb/mark-all-backuped-files ()
  "mark files in the current dired buffer with
* if part of the backup
% partially backed up (only folders may have this mark)
  (no mark) if not part of the backup"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let* ((tagged-file-name-list
            (duplicity--map-files-covered-by-backup
             "~/.duplicity/data.exclude.list.txt"
             (gb/get-list-of-files-in-current-dired-mode))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (ignore-errors
            (end-of-line)
            (let ((file-name (duplicity--get-file-name-at-point)))
              (when (or (file-exists-p file-name) (file-directory-p file-name))
                (when-let ((stat (duplicity--get-backup-status-from-definition file-name)))
                  (cond ((eq t stat)
                         (diredp-mark-with-char "*" )
                         (previous-line))
                        ((eq 'partial stat)
                         (diredp-mark-with-char "%" )
                         (previous-line)))))))
          (forward-line))))))

(bind-key "C-c b I" #'gb/mark-all-backuped-files dired-mode-map)

(defun duplicity--file-status (file-name)  
  (let ((abs-file-name (expand-file-name file-name)))
    (if (not (file-exists-p (format "/run/media/%s/%s/data-backup" (user-login-name) duplicity-mount)))
        "backup medium not accessible or present"
      (if (file-directory-p abs-file-name)
          "no backup info for folders available"
        (if (not (string-prefix-p (format "/home/%s/" (user-login-name)) abs-file-name))          
            (format "no info about file '%s' or not part of the backup" abs-file-name)
          (shell-command-to-string (format "duplicity collection-status --file-changed %s \"file:///run/media/%s/%s/data-backup\""
                                           (string-remove-prefix (format "/home/%s/" (user-login-name)) abs-file-name)
                                           (user-login-name)
                                           duplicity-mount)))))))

(defun duplicity--note-popup (position text)
  (setq dired-annotator--note-should-not-popup t) ;; next call should not reopen note again, but hide it
  (let ((buffer (get-buffer-create "* backup file status *")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text))
    (posframe-show
     buffer
     :position position :lines-truncate t
     ;; :width 80
     :background-color "#202020"
     :border-color "plum4" :border-width 2 :left-fringe 3 :right-fringe 3)
    (add-hook 'pre-command-hook #'duplicity--remove-note-popup)))

(defun duplicity--get-file-name-at-point ()
  (call-interactively #'diredp-copy-abs-filenames-as-kill)
  diredp-last-copied-filenames)

(defun duplicity--remove-note-popup ()
  "called to remove popup"
  (remove-hook 'pre-command-hook #'duplicity--remove-note-popup)
  (posframe-hide-all))

(defun duplicity--find-backup-status-from-definition (file-name)
  "try to find the backup status for the given FILE-NAME based on the backup definition"
  (cl-find-if
   (lambda (pair) (string-equal
              (duplicity--normalize-fs-name-for-prefix-check file-name)
              (cdr pair)))
   (duplicity--map-files-covered-by-backup
    "~/.duplicity/data.exclude.list.txt"
    (list (duplicity--normalize-fs-name-for-prefix-check file-name)))))

(defun duplicity--get-backup-status-from-definition (file-name)
  "try to find the backup status only for the given FILE-NAME based on the backup definition"
  (if-let ((found (duplicity--find-backup-status-from-definition file-name)))
      (car found)
    nil))

(defun duplicity--show-current-file-backup-info ()
  "show actual backup information for the file at point"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (ignore-errors
      (let ((file-name (duplicity--get-file-name-at-point))) ;; dired-x-guess-file-name-at-point
        (when (file-exists-p file-name)
          (duplicity--note-popup (point) (duplicity--file-status file-name)))))))

(bind-key "C-c b b" #'duplicity--show-current-file-backup-info dired-mode-map)

(defun duplicity--show-probable-backup-status ()
  "show whether the file at point was changed since last backup"
  (interactive)
  (when (eq major-mode 'dired-mode)    
    (let ((file-name (duplicity--get-file-name-at-point))) ;; dired-x-guess-file-name-at-point
      (when (file-exists-p file-name)
        (let ((file-access-str (format-time-string "%Y-%m-%d %T" (file-attribute-modification-time (file-attributes file-name))))
              (last-backup-str (format-time-string "%Y-%m-%d %T" (date-to-time (slurp "~/.duplicity/default.last_run") ))))
          (duplicity--note-popup
           (point)
           (format "%s\n(backup date %s, file date %s)"
                   (let ((is-backed-up (duplicity--get-backup-status-from-definition file-name)))
                     (cond ((and is-backed-up (string> file-access-str last-backup-str))
                            (propertize "last change NOT in backup" 'face '(:foreground "red" :weight bold)))
                           ((or (not is-backed-up) (string> file-access-str last-backup-str))
                            (propertize "last change NOT in backup (not even part of the backup)" 'face '(:foreground "red" :weight bold)))
                           (t (propertize "backup probably up to date" 'face '(:foreground "green" :weight bold)))))
                   last-backup-str
                   file-access-str)))))))

(provide 'duplicity-dired-backup-status)
