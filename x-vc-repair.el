;; x-vc-repair.el -   -*- lexical-binding: t -*-
;;
;; usage: (require 'x-vc-repair)
:end: ;; script-help
;;
;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; This file is part of Emacs Development.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; .:lst:. start repair
;; --------------------------------------------------
;; |||:sec:||| Repair bugs
;; --------------------------------------------------

(condition-case err
    (progn
      (require 'vc)
      (require 'vc-hooks)
      (require 'vc-dir)
      (require 'vc-cvs)
      (require 'vc-svn)
      ;; (require 'vc-bzr)
      (require 'vc-git)
      (require 'vc-hg)
      (require 'vc-mtn)
      )
  (error (message "error: %s (ignored)" (error-message-string err))))

;;;! Emacs 27
(unless (> emacs-major-version 26)

;; GNU bug report logs - #37182 24.5; 24.5.1: C-u vc-dir-mark-all-files should not mark directories
(defun vc-dir-mark-all-files (arg)
  "Mark all files with the same state as the current one.
With non-nil ARG (prefix argument, if interactive) mark all files.
If the current entry is a directory, mark all child files.

The commands operate on files that are on the same state.
This command is intended to make it easy to select all files that
share the same state."
  (interactive "P")
  (if arg
      ;; Mark all files.
      (progn
        ;; First check that no directory is marked, we can't mark
        ;; files in that case.
        (ewoc-map
         (lambda (filearg)
           (when (and (vc-dir-fileinfo->directory filearg)
                      (vc-dir-fileinfo->marked filearg))
             (error "Cannot mark all files, directory `%s' marked"
                    (vc-dir-fileinfo->name filearg))))
         vc-ewoc)
        (ewoc-map
         (lambda (filearg)
           (unless (or (vc-dir-fileinfo->directory filearg)
                       (vc-dir-fileinfo->marked filearg))
             (setf (vc-dir-fileinfo->marked filearg) t)
             t))
         vc-ewoc))
    (let* ((crt  (ewoc-locate vc-ewoc))
           (data (ewoc-data crt)))
      (if (vc-dir-fileinfo->directory data)
          ;; It's a directory, mark child files.
          (let (crt-data)
            (while (and (setq crt (ewoc-next vc-ewoc crt))
                        (setq crt-data (ewoc-data crt))
                        (not (vc-dir-fileinfo->directory crt-data)))
              (setf (vc-dir-fileinfo->marked crt-data) t)
              (ewoc-invalidate vc-ewoc crt)))
        ;; It's a file
        (let ((state (vc-dir-fileinfo->state data)))
          (setq crt (ewoc-nth vc-ewoc 0))
          (while crt
            (let ((crt-data (ewoc-data crt)))
              (when (and (not (vc-dir-fileinfo->marked crt-data))
                         (eq (vc-dir-fileinfo->state crt-data) state)
                         (not (vc-dir-fileinfo->directory crt-data)))
                (vc-dir-mark-file crt)))
            (setq crt (ewoc-next vc-ewoc crt))))))))

;; GNU bug report logs - #37185 24.5.1: vc--add-line, vc--remove-regexp are sub-optimal
;; Subroutine for `vc-default-ignore'
(defun vc--add-line (string file)
  "Add STRING as a line to FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote string) "$") nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert string "\n")
      (save-buffer))))

(defun vc--remove-regexp (regexp file)
  "Remove all matching for REGEXP in FILE."
  (if (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match ""))
        (save-buffer))))

;; GNU bug report logs - #37214 [PATCH] vc-svn error messages are used as ignore list
;; obsoleted by new ‘vc-gnore’ API
;; (defun vc-svn-ignore (file &optional directory remove _is-file)
;;   "Ignore FILE under Subversion.
;; FILE is a wildcard specification, either relative to
;; DIRECTORY or absolute."
;;   (let* ((path (directory-file-name (expand-file-name file directory)))
;;          (directory (file-name-directory path))
;;          (file (file-name-nondirectory path))
;;          (ignores (vc-svn-ignore-completion-table directory))
;;          (ignores (if remove
;;                       (delete file ignores)
;;                     (push file ignores))))
;;     (vc-svn-command nil 0 nil nil "propset" "svn:ignore"
;;                     (mapconcat #'identity ignores "\n")
;;                     directory)))

;; GNU bug report logs - #37216 [PATCH] vc-svn-ignore sets incorrect properties for relative filenames
(defun vc-svn-ignore-completion-table (directory)
  "Return the list of ignored files in DIRECTORY."
  (with-temp-buffer
    (if (= (vc-svn-command t t nil "propget" "svn:ignore" (expand-file-name directory)) 0)
        (split-string (buffer-string) "\n"))))

)
;;;! Emacs 27

;; |:here:| open

;;;! Emacs 24
(when (> emacs-major-version 24)

;; GNU bug report logs - #39380 26.3: Opening files in vc-dir-mode with differing root and working dir fails
(defun vc-hg-dir-status-files (dir files update-function)
  ;; XXX: We can't pass DIR directly to 'hg status' because that
  ;; returns all ignored files if FILES is non-nil (bug#22481).
  (let ((default-directory dir))
    (vc-hg-command (current-buffer) 'async files
                   "status" "re:" "-I" "."
                   (concat "-mardu" (if files "i"))
                   "-C"))
  (vc-run-delayed
    (vc-hg-after-dir-status update-function)))

)
;;;! Emacs 24

;;;! Emacs 23
(when (and (fboundp 'vc-git-root)
           ;; (> emacs-major-version 23)
           )

;; GNU bug report logs - #39452 [PATCH] vc-git-state fails for filenames with wildcards

;; See `Git - pathspec`_
;; .. _`Git - pathspec`: https://git-scm.com/docs/gitglossary.html#Documentation/gitglossary.txt-aiddefpathspecapathspec

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to vc-do-command is that this function always invokes
`vc-git-program'."
  (let ((coding-system-for-read
         (or coding-system-for-read vc-git-log-output-coding-system))
        (coding-system-for-write
         (or coding-system-for-write vc-git-commits-coding-system))
        (process-environment
         (append
          `("GIT_DIR"
            "GIT_LITERAL_PATHSPECS=1"
            ;; Avoid repository locking during background operations
            ;; (bug#21559).
            ,@(when revert-buffer-in-progress-p
                '("GIT_OPTIONAL_LOCKS=0")))
          process-environment)))
    (apply 'vc-do-command (or buffer "*vc*") okstatus vc-git-program
           ;; https://debbugs.gnu.org/16897
           (unless (and (not (cdr-safe file-or-list))
                        (let ((file (or (car-safe file-or-list)
                                        file-or-list)))
                          (and file
                               (eq ?/ (aref file (1- (length file))))
                               (equal file (vc-git-root file)))))
             file-or-list)
           (cons "--no-pager" flags))))

(defun vc-git--call (buffer command &rest args)
  ;; We don't need to care the arguments.  If there is a file name, it
  ;; is always a relative one.  This works also for remote
  ;; directories.  We enable `inhibit-nul-byte-detection', otherwise
  ;; Tramp's eol conversion might be confused.
  (let ((inhibit-nul-byte-detection t)
        (coding-system-for-read
         (or coding-system-for-read vc-git-log-output-coding-system))
        (coding-system-for-write
         (or coding-system-for-write vc-git-commits-coding-system))
        (process-environment
         (append
          `("GIT_DIR"
            "GIT_LITERAL_PATHSPECS=1"
            ;; Avoid repository locking during background operations
            ;; (bug#21559).
            ,@(when revert-buffer-in-progress-p
                '("GIT_OPTIONAL_LOCKS=0")))
          process-environment)))
    (apply 'process-file vc-git-program nil buffer nil "--no-pager" command args)))

)
;;;! Emacs 23

(condition-case err
    (progn
      (require 'vc-src)

;; GNU bug report logs - #39502 [PATCH] Use one src status -a call for vc-src-dir-status-files
(defun vc-src--parse-state (out)
  (when (null (string-match "does not exist or is unreadable" out))
    (let ((state (aref out 0)))
      (cond
       ;; FIXME: What to do about L code?
       ((eq state ?.) 'up-to-date)
       ((eq state ?A) 'added)
       ((eq state ?M) 'edited)
       ((eq state ?I) 'ignored)
       ((eq state ?R) 'removed)
       ((eq state ?!) 'missing)
       ((eq state ??) 'unregistered)
       (t 'up-to-date)))))

(defun vc-src-state (file)
  "SRC-specific version of `vc-state'."
  (let*
      ((status nil)
       (default-directory (vc-file-name-directory file nil t))
       (file (vc-file-relative-name file))
       (out
        (with-output-to-string
          (with-current-buffer
              standard-output
            (setq status
                  ;; Ignore all errors.
                  (condition-case nil
                      (process-file
                       vc-src-program nil t nil
                       "status" "-a" file)
                    (error nil)))))))
    (when (eq 0 status)
      (vc-src--parse-state out))))

(defun vc-src-dir-status-files (dir files update-function)
  (let*
      ((result nil)
       (status nil)
       (default-directory (or dir default-directory))
       (out
        (with-output-to-string
          (with-current-buffer
              standard-output
            (setq status
                  ;; Ignore all errors.
                  (condition-case nil
                      (apply
                       #'process-file vc-src-program nil t nil
                       "status" "-a"
                       (mapcar (lambda (f) (vc-file-relative-name f)) files))
                    (error nil))))))
       dlist)
    (when (eq 0 status)
      (dolist (line (split-string out "[\n\r]" t))
        (let* ((pair (split-string line "[\t]" t))
               (state (vc-src--parse-state (car pair)))
               (frel (cadr pair)))
          (if (file-directory-p frel)
              (push frel dlist)
            (when (not (eq state 'up-to-date))
              (push (list frel state) result)))
          ))
      (dolist (drel dlist)
        (let* ((dresult (vc-src-dir-status-files (expand-file-name drel) nil #'identity)))
          (dolist (dres dresult)
            (push (list (concat (file-name-as-directory drel) (car dres)) (cadr dres)) result)))
            )
      (funcall update-function result))))

;; |:here:||:todo:| unreported: SRC commands do not work in sub-directories
(defcustom vc-src-command-safe t
  "Run SRC commands separately and normalized for each file.
This is necessary when SRC has trouble working on files in
sub-directories."
  :type 'boolean
  :group 'vc-src)

(defun vc-src-command-raw (buffer file-or-list flags)
  "A wrapper around ‘vc-do-command’ for use in vc-src.el.
This function differs from ‘vc-do-command’ in that it invokes `vc-src-program'."
  (let (file-list)
    (cond ((stringp file-or-list)
           (setq file-list (list "--" file-or-list)))
          (file-or-list
           (setq file-list (cons "--" file-or-list))))
    (apply 'vc-do-command (or buffer "*vc*") 0 vc-src-program file-list flags)))

(defun vc-src-command-iterator (buffer file-or-list flags &optional dir-is-empty)
  "A wrapper around ‘vc-do-command-raw’ for use in vc-src.el.

If ‘vc-src-command-safe’ is non-nil, ‘vc-do-command-raw’ is
applied in BUFFER to each file from FILE-OR-LIST.  File is
normalized, such that it becomes a simple basename relative to
‘default-directory’.

If a file is a directory and DIR-IS-EMPTY is nil,
‘default-directory’ is set to file and the command is run without
a file argument."
  (if (and vc-src-command-safe file-or-list)
      (dolist (file (if (stringp file-or-list) (list file-or-list) file-or-list))
        (let* ((default-directory (vc-file-name-directory file nil (not dir-is-empty)))
               (file (vc-file-relative-name file nil dir-is-empty)))
          (vc-src-command-raw buffer file flags)))
    (vc-src-command-raw buffer file-or-list flags)))

(defun vc-src-command (buffer file-or-list &rest flags)
  "A wrapper around ‘vc-src-command-iterator’ with DIR-IS-EMPTY nil."
  (vc-src-command-iterator buffer file-or-list flags))

(defun vc-src-command-dir-empty (buffer file-or-list &rest flags)
  "A wrapper around ‘vc-src-command-iterator’ with DIR-IS-EMPTY t."
  (vc-src-command-iterator buffer file-or-list flags t))

(defun vc-src-print-log (files buffer &optional shortlog _start-revision limit)
  "Print commit log associated with FILES into specified BUFFER.
If SHORTLOG is non-nil, use the list method.
If START-REVISION is non-nil, it is the newest revision to show.
If LIMIT is non-nil, show no more than this many entries."
  ;; FIXME: Implement the range restrictions.
  ;; `vc-do-command' creates the buffer, but we need it before running
  ;; the command.
  (vc-setup-buffer buffer)
  ;; If the buffer exists from a previous invocation it might be
  ;; read-only.
  (let ((inhibit-read-only t))
    (with-current-buffer
        buffer
      (apply 'vc-src-command-dir-empty buffer files (if shortlog "list" "log")
             (nconc
              ;;(when start-revision (list (format "%s-1" start-revision)))
              (when limit (list "-l" (format "%s" limit)))
              vc-src-log-switches)))))

      )
  (error (message "error: %s (ignored)" (error-message-string err))))

;; |:here:||:todo:| unreported

;; |:here:| missing find-ignore-file functions
(unless (fboundp 'vc-bzr-find-ignore-file)
(defun vc-bzr-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".bzrignore"
                    (vc-bzr-root file)))
)

(unless (fboundp 'vc-git-find-ignore-file)
(defun vc-git-find-ignore-file (file)
  "Return the git ignore file that controls FILE."
  (expand-file-name ".gitignore"
                    (vc-git-root file)))
)

(unless (fboundp 'vc-hg-find-ignore-file)
(defun vc-hg-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".hgignore"
                    (vc-hg-root file)))
)

(if (t) nil                             ; disabled

;; |:here:| unreported: vc-revert does not work for added files under Git
;; |:todo:| it happens sporadically, that ‘vc-backend’ reports nil, but
;; ‘vc-responsible-backend’ reports the correct backend, the reason is
;; unclear

(defmacro vc-call-responsible (fun file &rest args)
  "A convenience macro for calling VC backend functions.
Functions called by this macro must accept FILE as the first argument.
ARGS specifies any additional arguments.  FUN should be unquoted.
BEWARE!! FILE is evaluated twice!!"
  `(vc-call-backend (vc-responsible-backend ,file) ',fun ,file ,@args))

(defun vc-version-backup-file (file &optional rev)
  "Return name of backup file for revision REV of FILE.
If version backups should be used for FILE, and there exists
such a backup for REV or the working revision of file, return
its name; otherwise return nil."
  (when (vc-call-responsible make-version-backups-p file)
    (let ((backup-file (vc-version-backup-file-name file rev)))
      (if (file-exists-p backup-file)
          backup-file
        ;; there is no automatic backup, but maybe the user made one manually
        (setq backup-file (vc-version-backup-file-name file rev 'manual))
        (when (file-exists-p backup-file)
          backup-file)))))

(defun vc-revert-file (file)
  "Revert FILE back to the repository working revision it was based on."
  (with-vc-properties
   (list file)
   (let ((backup-file (vc-version-backup-file file)))
     (when backup-file
       (copy-file backup-file file 'ok-if-already-exists)
       (vc-delete-automatic-version-backups file))
     (vc-call-responsible revert file backup-file))
   `((vc-state . up-to-date)
     (vc-checkout-time . ,(file-attribute-modification-time
                           (file-attributes file)))))
  (vc-resynch-buffer file t t))

)
;; .:lst:. end repair

;; |:here:|

;; .:lst:. start ui
;; --------------------------------------------------
;; |||:sec:||| User Interface
;; --------------------------------------------------

(defalias 'vc-ignore-pattern 'vc-ignore
  "Ignore PATTERN under VCS of DIRECTORY.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

PATTERN is an expression following the rules of the backend
pattern syntax, matching the files to be ignored.  When REMOVE is
non-nil, remove PATTERN from the list of ignored files.

When called interactively, prompt for a PATTERN to ignore, unless
a prefix argument is given, in which case prompt for a PATTERN to
remove. The completion collection contains the currently defined
patterns from the ignore file.")

(defun vc-ignore-file (file &optional directory remove prompt)
  "Ignore FILE under VCS of DIRECTORY.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

If FILE is nil, ‘vc-ignore-fileset’ is called.

Otherwise, FILE is an unescaped file path.  The directory name of
FILE expanded against DIRECTORY is used to determine the ignore
file.  The effective pattern consists of the file path relative
to the directory of the ignore file, properly escaped and
anchored by the VC backend.

The effective pattern is added to the list of ignored files,
unless REMOVE is non-nil, in which case it is removed.

When called interactively and the mode is neither ‘vc-dir-mode’
nor ‘dired-mode’, prompt for a FILE to ignore, unless a prefix
argument is given, in which case prompt for a FILE to remove from
the list of ignored files.

PROMPT is passed on to ‘vc-ignore-fileset’. When called
interatively, PROMPT is set to ‘t’."
  (interactive
   (list
    (unless (or (derived-mode-p 'vc-dir-mode) (derived-mode-p 'dired-mode))
      (read-file-name
       (concat "File to "
               (if (not current-prefix-arg) "ignore" "remove") ": ")))
    nil current-prefix-arg (derived-mode-p 'dired-mode)))
  (if file
      (vc-ignore file directory remove t)
    (vc-ignore-fileset nil remove prompt)))

;; .:lst:. end ui
;; .:lst:. start frontend
;; --------------------------------------------------
;; |||:sec:||| Frontend
;; --------------------------------------------------

(defun vc-ignore (pattern-or-file &optional directory remove is-file)
  "Ignore PATTERN-OR-FILE under VCS of DIRECTORY.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

When REMOVE is non-nil, remove PATTERN-OR-FILE from the list of
ignored files.

If IS-FILE is nil, PATTERN-OR-FILE is considered a pattern that
should not be modified.  DIRECTORY is used to determine the
ignore file.

If IS-FILE is non-nil, PATTERN-OR-FILE is a considered a file
path that must be escaped and anchored.  The directory name of
PATTERN-OR-FILE expanded against DIRECTORY is used to determine
the ignore file.  The effective pattern consists of the file path
relative to the directory of the ignore file, properly escaped
and anchored by the VC backend."
  (interactive
   (let* ((dir default-directory)
          (backend (or (vc-responsible-backend dir)
                       (error "Unknown backend")))
          (is-dired-mode (derived-mode-p 'dired-mode))
          (is-vc-dir-mode (derived-mode-p 'vc-dir-mode))
          (ignore-param
           (if (not (or is-dired-mode is-vc-dir-mode))
               (list nil nil (vc-call-backend backend 'find-ignore-file dir) nil)
             (let* ((cur-file (or
                               (and is-vc-dir-mode (vc-dir-current-file))
                               (and is-dired-mode (car (dired-get-marked-files nil t)))))
                    ;; (vc-default-get-ignore-file-and-pattern 'Hg "" nil t nil)
                    (ip (vc-call-backend backend 'get-ignore-file-and-pattern cur-file dir t nil))
                    (cur-file-rel (file-relative-name cur-file (file-name-directory (cadr ip)))))
               (cons cur-file-rel ip))))
          (default-pattern (cadr ignore-param))
          (ignore-file (nth 2 ignore-param))
          (ignore-completion-table
           (delq nil (append  (list (and default-pattern "") (car ignore-param))
                              (vc-call-backend backend 'ignore-completion-table dir))))
          (remove current-prefix-arg))
     (list
      (completing-read
       (format "%s pattern verbatim %s %s: "
               (if remove "Remove" "Add")
               (if remove "from" "to")
               (file-relative-name ignore-file dir))
       ignore-completion-table nil nil
       default-pattern)
      nil remove nil)))
  (setq directory (or directory default-directory))
  (vc-call-backend (or (vc-responsible-backend directory)
                       (error "Unknown backend"))
                   'ignore pattern-or-file directory remove is-file))

(defvar vc--ignore-fileset-po
  '(("Remove" "Removing" "removed from ignored files" " from ignored files")
    ("Ignore" "Ignoring" "ignored" ""))
  "Alternate message strings for ‘vc-ignore-fileset’.")

(defun vc-ignore-fileset (&optional vc-fileset remove prompt)
  "Ignore file set under a version control system..

If VC-FILESET is not given, it is deduced with
‘vc-deduce-fileset’.

When REMOVE is non-nil, remove the files from the list of ignored
files.

If PROMPT is non-nil, confirm the operation. If the confirmation
is negative, do not perform the ignore operation."
  (let* ((fileset-arg (or vc-fileset (vc-deduce-fileset t t)))
         (backend (car fileset-arg))
         (files (delq nil (nth 1 fileset-arg)))
         (msg-strings (if remove
                          (car vc--ignore-fileset-po)
                        (cadr vc--ignore-fileset-po)))
         (msg (concat "No files " (nth 2 msg-strings))))
    (when (and files
               (or (not prompt)
                   (let ((files (nreverse
                                 (mapcar #'dired-make-relative files))))
                     (dired-mark-pop-up
                      " *Ignored files*" 'ignore files #'y-or-n-p
                      (format "%s %s%s "
                              (car msg-strings)
                              (dired-mark-prompt nil files)
                              (nth 3 msg-strings)
                              )))))
      (setq msg (concat (message "%s %s%s... " (nth 1 msg-strings) files
                                 (nth 3 msg-strings)) "done"))
      (mapc
       (lambda (file)
         (vc-call-backend backend 'ignore file nil remove t)
         (vc-dir-resynch-file file))
       files))
    (when (derived-mode-p 'vc-dir-mode)
      (vc-dir-move-to-goal-column))
    (message msg)))

;; .:lst:. end frontend
;; .:lst:. start default
;; --------------------------------------------------
;; |||:sec:||| Default
;; --------------------------------------------------

(defun vc-default-ignore (backend pattern-or-file &optional directory remove is-file)
  ;; implements ‘vc-ignore’ generically
  (apply #'vc-call-backend backend 'modify-ignores
         (vc-call-backend backend 'get-ignore-file-and-pattern
                          pattern-or-file directory is-file remove)))

(defun vc-expand-file-name (file &optional directory)
  " Call ‘expand-file-name’ with normalized FILE and DIRECTORY.

Avoids removing the final slash of directories from the
expansion, if FILE does not have a trailing slash."
  (if (or (string= file "")
          (string= file ".")
          (string= file "..")
          (and (>= (length file) 2)
               (or (string= (substring file -2) "/.")
                   (and (>= (length file) 3) (string= (substring file -3) "/..")))))
      (setq file (file-name-as-directory file)))
  (setq file (expand-file-name file directory))
  (if (and (not (vc-has-final-slash file))
           (file-directory-p file))
      (setq file (file-name-as-directory file)))
  file)

;; (insert (format " ;; => %S" (vc-expand-file-name "xx/"    "/some/dir/"))) ;; => "/some/dir/xx/"
;; (insert (format " ;; => %S" (vc-expand-file-name ""       "/some/dir/"))) ;; => "/some/dir/"
;; (insert (format " ;; => %S" (vc-expand-file-name "."      "/some/dir/"))) ;; => "/some/dir/"
;; (insert (format " ;; => %S" (vc-expand-file-name ".."     "/some/dir/"))) ;; => "/some/"
;; (insert (format " ;; => %S" (vc-expand-file-name "zz/./"  "/some/dir/"))) ;; => "/some/dir/zz/"
;; (insert (format " ;; => %S" (vc-expand-file-name "zz/."   "/some/dir/"))) ;; => "/some/dir/zz/"
;; (insert (format " ;; => %S" (vc-expand-file-name "zz/.."  "/some/dir/"))) ;; => "/some/dir/"

;; (insert (format " ;; => %S" (vc-expand-file-name "/usr/local"  "/some/dir/"))) ;; => "/usr/local/"

;; don't worry about the final slash of DIRECTORY, results are identical:
;; (insert (format " ;; => %s" (equal (expand-file-name "xx/" "/some/dir/") (expand-file-name "xx/" "/some/dir")))) ;; => t

;; Here are various effects with ‘expand-file-name’:
;; (insert (format " ;; => %S" (expand-file-name "xx/" "/some/dir/"))) ;; => "/some/dir/xx/"
;; (insert (format " ;; => %S" (expand-file-name "xx"  "/some/dir/"))) ;; => "/some/dir/xx"
;; (insert (format " ;; => %S" (expand-file-name ""    "/some/dir/"))) ;; => "/some/dir"
;; (insert (format " ;; => %S" (expand-file-name "."   "/some/dir/"))) ;; => "/some/dir"
;; (insert (format " ;; => %S" (expand-file-name ".."  "/some/dir/"))) ;; => "/some"
;; (insert (format " ;; => %S" (expand-file-name "/"   "/some/dir/"))) ;; => "/"
;; (insert (format " ;; => %S" (expand-file-name "./"  "/some/dir/"))) ;; => "/some/dir/"
;; (insert (format " ;; => %S" (expand-file-name "../" "/some/dir/"))) ;; => "/some/"

;; and empty string results in "./"
;; (insert (format " ;; => %S" (file-name-as-directory "" ))) ;; => "./"

(defun vc-default-get-ignore-file-and-pattern (backend pattern-or-file &optional directory is-file remove)
  "Determine ignore file and pattern for BACKEND from PATTERN-OR-FILE.
Implements API of ‘vc-ignore’ for PATTERN-OR-FILE, DIRECTORY and IS-FILE.
REMOVE is passed through without evaluation.
Returns (pattern ignore-file remove) suitable for calling
‘vc-default-modify-ignores’."

  (if (null pattern-or-file) (setq pattern-or-file ""))
  (setq directory (or directory default-directory))
  (when is-file
    (setq pattern-or-file (vc-expand-file-name pattern-or-file directory))
    ;; apply directory-as-file-name, otherwise, if pattern-or-file was
    ;; a sub-repository, find-ignore-file would return the wrong
    ;; ignore file:
    ;; (vc-cvs-find-ignore-file "/re/po/dir/") => /re/po/dir/.cvsignore
    ;; (vc-cvs-find-ignore-file "/re/po/dir") => /re/po/.cvsignore
    (if (not (string= pattern-or-file directory))
        (setq directory (file-name-directory (directory-file-name pattern-or-file)))))

  (let* ((ignore-file (vc-call-backend backend 'find-ignore-file directory))
         (ignore-dir (file-name-directory ignore-file))
         is-dir ignore-param pattern)
    (if (not is-file)
        (setq ignore-param vc-ignore-param-none)

      ;; prepare file pattern
      (let* ((ignore-dir-len (length ignore-dir))
             (file-len (length pattern-or-file)))
        (unless (cond
                 ((>= file-len ignore-dir-len)
                  (string= (substring pattern-or-file 0 ignore-dir-len) ignore-dir))
                 ((= (1- ignore-dir-len) file-len)
                  (string= pattern-or-file (substring ignore-dir 0 file-len))))
          (error "Ignore spec %s is not below project root %s"
                 pattern-or-file ignore-dir))
        ;; directory may not yet exist
        (setq is-dir (or (vc-has-final-slash pattern-or-file)
                         (file-directory-p pattern-or-file)))
        (setq pattern-or-file
              (directory-file-name
               (substring (if is-dir
                              (file-name-as-directory pattern-or-file)
                            pattern-or-file)
                          ignore-dir-len)))
        ;; (setq debug-on-next-call t) ;; |||:here:|||
        (if (string= pattern-or-file "") (setq is-dir nil))
        (setq ignore-param (vc-call-backend backend 'ignore-param ignore-file))))
    (setq pattern
          (concat
           (plist-get ignore-param :anchor:)
           (funcall (or (plist-get ignore-param :escape:) #'identity)
                    pattern-or-file)
           (or (and is-dir (plist-get ignore-param :dir-trailer:))
               (plist-get ignore-param :trailer:))))
    (list pattern ignore-file remove)))

(defun vc-default-modify-ignores (_backend pattern ignore-file remove)
  "Add PATTERN to IGNORE-FILE, if REMOVE is nil..
Otherwise remove PATTERN from IGNORE-FILE."
  (if remove
      (vc--remove-regexp
       (concat "^" (regexp-quote pattern) "\\(\n\\|$\\)") ignore-file)
    (vc--add-line pattern ignore-file)))

(defun vc-file-name-directory (file &optional dir dir-as-file)
  "Get directory name for FILE.
FILE is expanded against DIR.  If FILE is a directory and DIR-AS-FILE
is non-nil, its parent directory is returned."
  (and file
       (let* ((path (expand-file-name file dir)))
         (file-name-directory
          (if dir-as-file
              (directory-file-name path)
            path)))))

(defun vc-file-relative-name (file &optional dir dir-is-empty)
  "Get relative file name for FILE against DIR.
If FILE is a directory and DIR-IS-EMPTY is non-nil, nil is returned.
Otherwise, if FILE is a directory, the final slash is removed."
  (and (not (and dir-is-empty (file-directory-p file)))
       (directory-file-name (file-relative-name file dir))))

(defun vc-has-final-slash (s)
  ;"Return index of final slash in string S or nil."
  (let ((l (1- (length s))))
    (and (> l 0) (eq (aref s l) ?/) l)))

;; .:lst:. end default
;; .:lst:. start generic ignore
;; --------------------------------------------------
;; |||:sec:||| Generic ignore parameters
;; --------------------------------------------------

(defvar vc-ignore-param-none
  '(:escape: identity :anchor: "" :trailer: "" :dir-trailer: "")
  "Property list of ignore parameters for plain strings.

All properties are optional.

Property :escape: is a function that takes a pattern string as parameter
and returns an escaped pattern (default is ‘identity’).

Property :anchor: is a string that is prepended to the ignore
pattern (default is an empty string).

Property :trailer: is a string that is appended to non-directory
ignore patterns (default is an empty string).

Property :dir-trailer: is a string that is appended to directory
ignore patterns (default is an empty string).")

(defvar vc-ignore-param-glob
  '(:escape: vc-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for unanchored glob wildcards.")

(defvar vc-ignore-param-glob-anchored
  '(:escape: vc-glob-escape :anchor: "/" :trailer: "" :dir-trailer: "/")
  "Ignore parameters for anchored glob wildcards.")

(defvar vc-ignore-param-regexp
  '(:escape: regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for anchored regular expressions.")

(defun vc-default-ignore-param (_backend &optional _ignore-file)
  "Default ignore parameters for IGNORE-FILE."
  vc-ignore-param-glob)

(defun vc-glob-escape (string)
  "Escape special glob characters in STRING."
  (save-match-data
    (if (string-match "[\\?*[]" string)
        (mapconcat (lambda (c)
                     (pcase c
                       (?\\ "\\\\")
                       (?? "\\?")
                       (?* "\\*")
                       (?\[ "\\[")
                       (_ (char-to-string c))))
                   string "")
      string)))
;; (vc-glob-escape "full[glo]?\\b*")

;; optimized code Python >= v3.7
;; # SPECIAL_CHARS
;; # closing ')', '}' and ']'
;; # '-' (a range in character set)
;; # '&', '~', (extended character set operations)
;; # '#' (comment) and WHITESPACE (ignored) in verbose mode
;; _special_chars_map = {i: '\\' + chr(i) for i in b'()[]{}?*+-|^$\\.&~# \t\n\r\v\f'}

(defvar vc--py-regexp-special-chars
  (mapcar
   (function
    (lambda (c)
      (cons c (concat "\\" (char-to-string c)))))
   "()[]{}?*+-|^$\\.&~# \t\n\r\v\f")
  "Characters that have special meaning in Python regular expressions.")
;; (cdr (assq ?/ vc--py-regexp-special-chars))
;; (cdr (assq ?\( vc--py-regexp-special-chars))

(defun vc-py-regexp-quote (string)
  "Python regexp to match exactly STRING and nothing else.
Ported from Python v3.7"
  (mapconcat
   (function
    (lambda (c)
      (or (cdr (assq c vc--py-regexp-special-chars))
          (char-to-string c))))
   string ""))
;; (insert (format " ;; %S" (vc-py-regexp-quote "abc+.?.\\g'\"hi\030|()"))) ;; "abc\\+\\.\\?\\.\\\\g'\"hi\\|\\(\\)"
;; (insert (format " ;; %S" (regexp-quote       "abc+.?.\\g'\"hi\030|()"))) ;; "abc\\+\\.\\?\\.\\\\g'\"hi|()"

;; .:lst:. end generic ignore
;; .:lst:. start cvs ignore
;; --------------------------------------------------
;; |||:sec:||| CVS specialized parameters
;; --------------------------------------------------

;; (require 'vc-cvs)
(fmakunbound 'vc-cvs-ignore)
(fmakunbound 'vc-cvs-append-to-ignore)
(put 'CVS 'vc-functions nil)

(defun vc-cvs-find-ignore-file (file)
  "Return the ignore file for FILE."
  (expand-file-name ".cvsignore" (if file (file-name-directory file))))

(defvar vc-cvs-ignore-param-glob
  '(:escape: vc-cvs-glob-escape :anchor: "" :trailer: "" :dir-trailer: "/")
  "Ignore parameters for CVS partially anchored glob wildcards.")

(defun vc-cvs-ignore-param (&optional _ignore-file)
  "Appropriate CVS ignore parameters for IGNORE-FILE."
  vc-cvs-ignore-param-glob)

(defun vc-cvs-glob-escape (string)
  "Escape special glob characters and spaces in STRING."
  (replace-regexp-in-string " " "?" (vc-glob-escape string) t))

;; .:lst:. end cvs ignore
;; .:lst:. start svn ignore
;; --------------------------------------------------
;; |||:sec:||| SVN specialized parameters
;; --------------------------------------------------

;; (require 'vc-svn)
(fmakunbound 'vc-svn-ignore)
(put 'SVN 'vc-functions nil)

(defun vc-svn-find-ignore-file (file)
  "Return the virtual ignore file for FILE."
  (expand-file-name ".svnignore" (if file (file-name-directory file))))

(defvar vc-svn-ignore-param-glob
  '(:escape: vc-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for SVN unanchored glob wildcards.")

(defun vc-svn-ignore-param (&optional _ignore-file)
  "Appropriate SVN ignore parameters for IGNORE-FILE."
  vc-svn-ignore-param-glob)

(defun vc-svn-modify-ignores (pattern ignore-file remove)
  ;; implements ‘vc-default-modify-ignores’ for SVN
  (let* ((directory (file-name-directory ignore-file))
         (ignores (vc-svn-ignore-completion-table directory))
         (ignores (if remove
                      (delete pattern ignores)
                    (push pattern ignores))))
    (vc-svn-command nil 0 nil nil "propset" "svn:ignore"
                    (mapconcat #'identity ignores "\n")
                    directory)))

;; .:lst:. end svn ignore
;; .:lst:. start src ignore
;; --------------------------------------------------
;; |||:sec:||| SRC specialized parameters
;; --------------------------------------------------

;; (require 'vc-src)
(put 'SRC 'vc-functions nil)

(defun vc-src-find-ignore-file (file)
  "Return the ignore file for FILE."
  (expand-file-name ".srcignore" (if file (file-name-directory file))))

(defun vc-src-glob-escape (string)
  "Escape special glob characters in STRING."
  (save-match-data
    (if (string-match "[?*[]" string)
        (mapconcat (lambda (c)
                     (pcase c
                       (?? "[?]")
                       (?* "[*]")
                       (?\[ "[[]")
                       (_ (char-to-string c))))
                   string "")
      string)))
;; (vc-src-glob-escape "full[glo]?\\b*")

(defvar vc-src-ignore-param-glob
  '(:escape: vc-src-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for SRC unanchored glob wildcards.")

(defun vc-src-ignore-param (&optional _ignore-file)
  "Appropriate SRC ignore parameters for IGNORE-FILE."
  vc-src-ignore-param-glob)

;; .:lst:. end src ignore
;; .:lst:. start bzr ignore
;; --------------------------------------------------
;; |||:sec:||| Bzr specialized parameters
;; --------------------------------------------------

(put 'Bzr 'vc-functions nil)
(put 'BZR 'vc-functions nil)

(defvar vc-bzr-ignore-param-regexp
  '(:escape: vc-py-regexp-quote :anchor: "RE:^" :trailer: "$" :dir-trailer: "/.*")
  "Ignore parameters for Bzr anchored regular expressions.")

(defun vc-bzr-ignore-param (&optional _ignore-file)
  "Appropriate Bzr ignore parameters for IGNORE-FILE."
        vc-bzr-ignore-param-regexp)

;; .:lst:. end bzr ignore
;; .:lst:. start git ignore
;; --------------------------------------------------
;; |||:sec:||| Git specialized parameters
;; --------------------------------------------------

(put 'Git 'vc-functions nil)
(put 'GIT 'vc-functions nil)

(defun vc-git-ignore-param (&optional _ignore-file)
  "Appropriate Git ignore parameters for IGNORE-FILE."
  vc-ignore-param-glob-anchored)

;; .:lst:. end git ignore
;; .:lst:. start hg ignore
;; --------------------------------------------------
;; |||:sec:||| Hg specialized parameters
;; --------------------------------------------------

;; (require 'vc-hg)
(fmakunbound 'vc-hg-ignore)
(put 'Hg 'vc-functions nil)
(put 'HG 'vc-functions nil)

(defvar vc-hg-ignore-param-regexp
  '(:escape: vc-py-regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for Hg anchored regular expressions.")

(defvar vc-hg-ignore-param-glob
  '(:escape: vc-glob-escape :anchor: "" :trailer: "" :dir-trailer: "/*")
  "Ignore parameters for Hg anchored regular expressions.")

(defun vc-hg-ignore-param (&optional ignore-file)
  "Appropriate Hg ignore parameters for IGNORE-FILE."
  (let ((syntax "regexp"))
    (if (not ignore-file)
        (setq ignore-file (vc-hg-find-ignore-file default-directory)))
    (if (file-exists-p ignore-file)
        (with-current-buffer (find-file-noselect ignore-file)
          (save-match-data
            (goto-char (point-max))
            (if (re-search-backward "^ *syntax: *\\(regexp\\|glob\\)$" nil t)
                (setq syntax (match-string 1))))))
    (if (string= syntax "regexp")
        vc-hg-ignore-param-regexp
      vc-hg-ignore-param-glob)))

;; .:lst:. end hg ignore
;; .:lst:. start mtn ignore
;; --------------------------------------------------
;; |||:sec:||| Mtn specialized parameters
;; --------------------------------------------------

(put 'Mtn 'vc-functions nil)
(put 'MTN 'vc-functions nil)

(unless (fboundp 'vc-mtn-find-ignore-file)
(defun vc-mtn-find-ignore-file (file)
  "Return the mtn ignore file that controls FILE."
  (expand-file-name ".mtn-ignore" (vc-mtn-root file)))
)

(defvar vc-mtn-ignore-param-regexp
  '(:escape: vc-py-regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for Mtn anchored regular expressions.")

(defun vc-mtn-ignore-param (&optional _ignore-file)
  "Appropriate Mtn ignore parameters for IGNORE-FILE."
  vc-mtn-ignore-param-regexp)

;; \|||:here:||||:todo:|

;; .:lst:. end mtn ignore
;; .:lst:. start integration
;; --------------------------------------------------
;; |||:sec:||| Integration
;; --------------------------------------------------

;; (require 'vc-hooks)
(define-key vc-prefix-map  "F" 'vc-ignore-file)
(define-key vc-prefix-map  "G" 'vc-ignore-pattern)

(bindings--define-key vc-menu-map [vc-ignore]
  '(menu-item "Ignore File..." vc-ignore-file
              :help "Ignore a file under current version control system"))
(bindings--define-key vc-menu-map [vc-ignore-pattern]
  '(menu-item "Ignore Pattern..." vc-ignore-pattern
              :help "Ignore a pattern under current version control system"))

;; (require 'vc-dir)

(defun vc-dir-ignore (&optional arg)
    "Ignore pattern.
If a prefix argument is given, remove pattern from ignore file.
See ‘vc-ignore-pattern’ for details.

For ignoring marked files in ‘vc-dir-mode’ see ‘vc-ignore-file’."
    (interactive "P")
    (unwind-protect
        (call-interactively 'vc-ignore-pattern)
      (with-temp-message "Use ‘F’ to ignore marked files." (sit-for 2))))

(define-key vc-dir-mode-map  "F" 'vc-ignore-file)
(define-key vc-dir-mode-map  "G" 'vc-dir-ignore)

;; .:lst:. end integration
  ;; |||:here:|||

(provide 'x-vc-repair)

;; 
;; :ide-menu: Emacs IDE Main Menu - Buffer @BUFFER@
;; . M-x `eIDE-menu' ()(eIDE-menu "z")

;; :ide: SHELL: safe-local-variable list of defined variables
;; . (shell-command (concat "grep -e '^(defvar' < " (file-name-nondirectory (buffer-file-name)) " | awk '{ print \"    (\" $2 \" . booleanp)\"; }'" ))

;;; :ide: DBG-VALUE: Read var/defun, maybe args, insert eIDE entry for viewing var or funcall results
;;; . (let ((sym (let ((v (variable-at-point nil)) (f (function-called-at-point))) (if (eq v 0) (setq v nil)) (cond ((and v f) (if (y-or-n-p (format "y: `%s' / n: `%s' " f v)) f v)) (f f) (v v) (t (symbol-at-point) nil)))) sym-str) (and sym (setq sym-str (symbol-name sym))) (let ((dbg-name (completing-read-fsf-compat "Symbol (defun/var) : " obarray (quote (lambda (s) (or (boundp s) (fboundp s)))) t sym-str nil nil)) dbg-sym) (setq dbg-sym (intern dbg-name)) (let ((dbg-args (and (fboundp dbg-sym) (describe-function dbg-sym) (read-from-minibuffer "Arguments: " nil nil nil nil "::fillme\::"))) (add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (describe-variable (quote query-replace-map)) (y-or-n-p (concat "add before DBG" "-VALUE entry? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (if (re-search-forward (concat "DBG" "-VALUE") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end))) (if (fboundp dbg-sym) (eIDE-insert-template (list "f" (format "DBG-FN: Show Result of `(%s %s)'" dbg-name dbg-args) (format "(let ((dbg-%s (%s %s))) (describe-variable 'dbg-%s))" dbg-name dbg-name dbg-args dbg-name))) (eIDE-insert-template (list "v" (format "DBG-VAR: Show Variable `%s'" dbg-name) (format "(describe-variable '%s)" dbg-name))))))))

;;; :ide: DBG-INSPECT-DEF: Define `wsx-inspect-var' and `wsx-inspect-expr'.
;;; . (progn (defmacro wsx-inspect-var (var) "Inspect value of VAR.\nThe value is shown with `describe-variable'.\n\nIf the `continue' prompt is ansered with `n',\n`debug-on-next-call' is set to `t'." (declare (indent 0)) (\` (save-window-excursion (describe-variable (quote (\, var))) (setq debug-on-next-call (eIDE-do-query 0 (quote (100 escape)) "Press d or ESC to debug, C-g to quit ..."))))) (defmacro wsx-inspect-expr (expr &optional name) "Inspect value of EXPR.\nIf optional argument NAME is nil, the variable name `debug-expr'\nis used.\n\nThe value is shown with `describe-variable'.\n\nIf the `continue' prompt is ansered with `n',\n`debug-on-next-call' is set to `t'." (declare (indent 0)) (or name (setq name (quote expr))) (let (dbg-var dbg-var-name) (setq dbg-var-name (format "wsx-dbg-%s" name)) (setq dbg-var (intern dbg-var-name)) (\` (save-window-excursion (let ((\, dbg-var)) (setq (\, dbg-var) (list (format "expression: %S expands to -->" (quote (\, expr))) (\, expr))) (describe-variable (quote (\, dbg-var))) (setq debug-on-next-call (eIDE-do-query 0 (quote (100 escape)) "Press d or ESC to debug, C-g to quit ..."))))))))

;;; :ide: DBG-INSPECT-IDE: Read SEXP, insert eIDE entry for viewing results
;;; . (let ((sexp (sexp-at-point)) sexp-str) (and sexp (setq sexp-str (format "%S" sexp))) (let ((dbg-sexp (completing-read-fsf-compat "Expression: " nil nil nil sexp-str nil nil))) (when (not (equal dbg-sexp "")) (let ((add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (y-or-n-p (concat "add before DBG" "-INSPECT entries? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (save-match-data (if (re-search-forward (concat "DBG" "-INSPECT-\\(RUN\\|IDE\\)") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end)))) (eIDE-insert-template (list "f" (format "DBG-SEXP: Show result of `%s'" (ea-text-summary dbg-sexp nil t)) (format "(let ((wsx-dbg-sexp (list (format \"expression: %%s expands to -->\" (quote %s)) %s))) (describe-variable 'wsx-dbg-sexp))" dbg-sexp dbg-sexp))))))))

;;; :ide: DBG-INSPECT-IDE: Read VARIABLE/DEFUN, maybe args, insert eIDE entry for viewing var or funcall results
;;; . (let ((sym (let ((v (variable-at-point nil)) (f (function-called-at-point))) (if (eq v 0) (setq v nil)) (cond ((and v f) (if (y-or-n-p (format "y: `%s' / n: `%s' " f v)) f v)) (f f) (v v) (t (symbol-at-point) nil)))) sym-str) (and sym (setq sym-str (symbol-name sym))) (let ((dbg-name (completing-read-fsf-compat "Symbol (defun/var) : " obarray (quote (lambda (s) (or (boundp s) (fboundp s)))) t sym-str nil nil)) dbg-sym) (setq dbg-sym (intern dbg-name)) (let ((dbg-args (and (fboundp dbg-sym) (describe-function dbg-sym) (read-from-minibuffer "Arguments: " nil nil nil nil "::fillme::"))) (add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (y-or-n-p (concat "add before DBG" "-VALUE entry? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (save-match-data (if (re-search-forward (concat "DBG" "-INSPECT-\\(RUN\\|IDE\\)") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end)))) (if (fboundp dbg-sym) (eIDE-insert-template (list "f" (format "DBG-FN: Show result of `%s'" (ea-text-summary (format "(%s %s)" dbg-name dbg-args) nil t)) (format "(let ((dbg-%s (%s %s))) (describe-variable 'dbg-%s))" dbg-name dbg-name dbg-args dbg-name))) (eIDE-insert-template (list "v" (format "DBG-VAR: Show Variable `%s'" dbg-name) (format "(describe-variable '%s)" dbg-name))))))))

;;; :ide: DBG-INSPECT-RUN: Show EXPRESSION at run-time
;;; . (let ((sexp (sexp-at-point)) sexp-str) (and sexp (setq sexp-str (format "%S" sexp))) (let ((dbg-sexp (completing-read-fsf-compat "Expression: " nil nil nil sexp-str nil nil))) (when (not (equal dbg-sexp "")) (forward-line 1) (let ((b (point))) (insert (format "(wsx-inspect-expr %s) ; |:debug\:|\n" dbg-sexp)) (indent-region b (point))))))

;;; :ide: DBG-INSPECT-RUN: Show VARIABLE at run-time
;;; . (let ((sym (variable-at-point 0)) sym-str) (if (eq sym 0) (setq sym nil)) (and sym (setq sym-str (symbol-name sym))) (let ((dbg-name (completing-read-fsf-compat "Variable : " obarray nil t sym-str nil nil)) dbg-sym) (when (not (equal dbg-name "")) (setq dbg-sym (intern dbg-name)) (forward-line 1) (let ((b (point))) (insert (format "(wsx-inspect-var %s) ; |:debug\:|\n" dbg-sym)) (indent-region b (point))))))

;;; :ide: +-#+
;;; . Inspect Expressions()

;; :ide: OCCUR-OUTLINE: entries: `ide-menu' delimiter: `:section:'
;; . (x-symbol-tag-occur-outline "ide-menu" '(":" ":") '(":" ":") "ide")

;; :ide: MENU-OUTLINE: entries: `ide-menu' delimiter: `:section:'
;; . (x-eIDE-menu-outline "ide-menu" '(":" ":") '(":" ":") "ide")

;; :ide: OCCUR-OUTLINE: sections: `sec' delimiter: `|:section:|'
;; . (x-symbol-tag-occur-outline "sec" '("|:" ":|"))

;; :ide: MENU-OUTLINE: sections: `sec' delimiter: `|:section:|'
;; . (x-eIDE-menu-outline "sec" '("|:" ":|"))

;; :ide: +-#+
;; . Buffer Outline ()

;;
;; Local Variables:
;; mode: emacs-lisp
;; truncate-lines: t
;; End:
