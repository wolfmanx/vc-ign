;;; vc-repair.el --- Repair various VC bugs -*- lexical-binding: t; -*-
;;
;; usage: (require 'vc-repair)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-ign
;; Package-Requires: ((emacs "24") ("vc-ign" "1.0.0"))

;; This file is part of VC Ignore.

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

;;; Commentary:
;;
;;  Backports various VC bug fixes from Emacs 27

;; .:lst:.  start repair
;; --------------------------------------------------
;; |||:sec:||| Repair bugs
;; --------------------------------------------------

;;; Code:

(let ((load-path
       (cons (file-name-directory
              (or load-file-name (buffer-file-name)))
             load-path)))
  (dolist (pkg '(vc vc-hooks vc-dir vc-cvs vc-svn vc-src vc-bzr vc-git vc-hg vc-mtn vc-ign))
    (condition-case err
        (require pkg)
      (error (message "error: %s (ignored)" (error-message-string err))))))

;;;! Emacs < 27
(when (< emacs-major-version 27)

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

(when (and (fboundp 'vc-git-root))

;; GNU bug report logs - #39452 [PATCH] vc-git-state fails for filenames with wildcards

;; See `Git - pathspec`_
;; .. _`Git - pathspec`: https://git-scm.com/docs/gitglossary.html#Documentation/gitglossary.txt-aiddefpathspecapathspec

(unless (boundp 'vc-git-commits-coding-system)
(defcustom vc-git-commits-coding-system 'utf-8
  "Default coding system for sending commit log messages to Git."
  :type '(coding-system :tag "Coding system to encode Git commit logs")
  :version "25.1"
  :group 'vc-git))

(unless (boundp 'vc-git-log-output-coding-system)
(defcustom vc-git-log-output-coding-system 'utf-8
  "Default coding system for receiving log output from Git."
  :type '(coding-system :tag "Coding system to decode Git log output")
  :version "25.1"
  :group 'vc-git))

(unless (boundp 'vc-git-program)
(defcustom vc-git-program "git"
  "Name of the Git executable (excluding any arguments)."
  :version "24.1"
  :type 'string
  :group 'vc-git))

(unless (boundp 'revert-buffer-in-progress-p)
(defvar revert-buffer-in-progress-p nil
  "Non-nil if a `revert-buffer' operation is in progress, nil otherwise."))

(unless (boundp 'inhibit-nul-byte-detection)
  (defvar inhibit-nul-byte-detection nil))

(unless (boundp 'inhibit-null-byte-detection)
  (defvar inhibit-null-byte-detection nil))

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
        (inhibit-null-byte-detection t)
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
    (apply 'process-file vc-git-program nil buffer nil "--no-pager" command args))))
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
    (vc-hg-after-dir-status update-function)))))
;;;! Emacs 24
;;;! Emacs 27

;; |:here:| open

(when (featurep 'vc-src)
(unless (fboundp 'vc-src--parse-state)

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
       (default-directory (vc-ign-file-name-directory file nil t))
       (file (vc-ign-file-relative-name file))
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
                       (mapcar (lambda (f) (vc-ign-file-relative-name f)) files))
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
              (push (list frel state) result)))))
      (dolist (drel dlist)
        (let* ((dresult (vc-src-dir-status-files (expand-file-name drel) nil #'identity)))
          (dolist (dres dresult)
            (push (list (concat (file-name-as-directory drel) (car dres)) (cadr dres)) result))))
      (funcall update-function result)))))

(unless (fboundp 'vc-src-command-raw)

;; |:here:||:todo:| reported to ESR,: SRC commands do not work in sub-directories
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
        (let* ((default-directory (vc-ign-file-name-directory file nil (not dir-is-empty)))
               (file (vc-ign-file-relative-name file nil dir-is-empty)))
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
              vc-src-log-switches)))))))

;; |:here:||:todo:| unreported

;; .:lst:. end repair
  ;; |||:here:|||

;; 
;; :ide-menu: Emacs IDE Main Menu - Buffer @BUFFER@
;; . M-x `eIDE-menu' ()(eIDE-menu "z")

;; :ide: SHELL: safe-local-variable list of defined variables
;; . (shell-command (concat "grep -e '^(defvar' < " (file-name-nondirectory (buffer-file-name)) " | awk '{ print \"    (\" $2 \" . booleanp)\"; }'" ))

;;; :ide: DBG-VALUE: Read var/defun, maybe args, insert eIDE entry for viewing var or funcall results
;;; . (let ((sym (let ((v (variable-at-point nil)) (f (function-called-at-point))) (if (eq v 0) (setq v nil)) (cond ((and v f) (if (y-or-n-p (format "y: `%s' / n: `%s'? " f v)) f v)) (f f) (v v) (t (symbol-at-point) nil)))) sym-str) (and sym (setq sym-str (symbol-name sym))) (let ((dbg-name (completing-read-fsf-compat "Symbol (defun/var) : " obarray (quote (lambda (s) (or (boundp s) (fboundp s)))) t sym-str nil nil)) dbg-sym) (setq dbg-sym (intern dbg-name)) (let ((dbg-args (and (fboundp dbg-sym) (describe-function dbg-sym) (read-from-minibuffer "Arguments: " nil nil nil nil "::fillme\::"))) (add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (describe-variable (quote query-replace-map)) (y-or-n-p (concat "add before DBG" "-VALUE entry? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (if (re-search-forward (concat "DBG" "-VALUE") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end))) (if (fboundp dbg-sym) (eIDE-insert-template (list "f" (format "DBG-FN: Show Result of `(%s %s)'" dbg-name dbg-args) (format "(let ((dbg-%s (%s %s))) (describe-variable 'dbg-%s))" dbg-name dbg-name dbg-args dbg-name))) (eIDE-insert-template (list "v" (format "DBG-VAR: Show Variable `%s'" dbg-name) (format "(describe-variable '%s)" dbg-name))))))))

;;; :ide: DBG-INSPECT-DEF: Define `wsx-inspect-var' and `wsx-inspect-expr'.
;;; . (progn (defmacro wsx-inspect-var (var) "Inspect value of VAR.\nThe value is shown with `describe-variable'.\n\nIf the `continue' prompt is ansered with `n',\n`debug-on-next-call' is set to `t'." (declare (indent 0)) (\` (save-window-excursion (describe-variable (quote (\, var))) (setq debug-on-next-call (eIDE-do-query 0 (quote (100 escape)) "Press d or ESC to debug, C-g to quit ..."))))) (defmacro wsx-inspect-expr (expr &optional name) "Inspect value of EXPR.\nIf optional argument NAME is nil, the variable name `debug-expr'\nis used.\n\nThe value is shown with `describe-variable'.\n\nIf the `continue' prompt is ansered with `n',\n`debug-on-next-call' is set to `t'." (declare (indent 0)) (or name (setq name (quote expr))) (let (dbg-var dbg-var-name) (setq dbg-var-name (format "wsx-dbg-%s" name)) (setq dbg-var (intern dbg-var-name)) (\` (save-window-excursion (let ((\, dbg-var)) (setq (\, dbg-var) (list (format "expression: %S expands to -->" (quote (\, expr))) (\, expr))) (describe-variable (quote (\, dbg-var))) (setq debug-on-next-call (eIDE-do-query 0 (quote (100 escape)) "Press d or ESC to debug, C-g to quit ..."))))))))

;;; :ide: DBG-INSPECT-IDE: Read SEXP, insert eIDE entry for viewing results
;;; . (let ((sexp (sexp-at-point)) sexp-str) (and sexp (setq sexp-str (format "%S" sexp))) (let ((dbg-sexp (completing-read-fsf-compat "Expression: " nil nil nil sexp-str nil nil))) (when (not (equal dbg-sexp "")) (let ((add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (y-or-n-p (concat "add before DBG" "-INSPECT entries? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (save-match-data (if (re-search-forward (concat "DBG" "-INSPECT-\\(RUN\\|IDE\\)") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end)))) (eIDE-insert-template (list "f" (format "DBG-SEXP: Show result of `%s'" (ea-text-summary dbg-sexp nil t)) (format "(let ((wsx-dbg-sexp (list (format \"expression: %%s expands to -->\" (quote %s)) %s))) (describe-variable 'wsx-dbg-sexp))" dbg-sexp dbg-sexp))))))))

;;; :ide: DBG-INSPECT-IDE: Read VARIABLE/DEFUN, maybe args, insert eIDE entry for viewing var or funcall results
;;; . (let ((sym (let ((v (variable-at-point nil)) (f (function-called-at-point))) (if (eq v 0) (setq v nil)) (cond ((and v f) (if (y-or-n-p (format "y: `%s' / n: `%s'? " f v)) f v)) (f f) (v v) (t (symbol-at-point) nil)))) sym-str) (and sym (setq sym-str (symbol-name sym))) (let ((dbg-name (completing-read-fsf-compat "Symbol (defun/var) : " obarray (quote (lambda (s) (or (boundp s) (fboundp s)))) t sym-str nil nil)) dbg-sym) (setq dbg-sym (intern dbg-name)) (let ((dbg-args (and (fboundp dbg-sym) (describe-function dbg-sym) (read-from-minibuffer "Arguments: " nil nil nil nil "::fillme::"))) (add-before (let ((query-replace-map (copy-keymap query-replace-map))) (define-key query-replace-map [return] (quote act)) (y-or-n-p (concat "add before DBG" "-VALUE entry? "))))) (save-excursion (when add-before (eIDE-jump-to-start) (save-match-data (if (re-search-forward (concat "DBG" "-INSPECT-\\(RUN\\|IDE\\)") nil t) (backward-symbol-tag 1 "ide" (quote (":" ":"))) (eIDE-jump-to-end)))) (if (fboundp dbg-sym) (eIDE-insert-template (list "f" (format "DBG-FN: Show result of `%s'" (ea-text-summary (format "(%s %s)" dbg-name dbg-args) nil t)) (format "(let ((dbg-%s (%s %s))) (describe-variable 'dbg-%s))" dbg-name dbg-name dbg-args dbg-name))) (eIDE-insert-template (list "v" (format "DBG-VAR: Show Variable `%s'" dbg-name) (format "(describe-variable '%s)" dbg-name))))))))

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

(provide 'vc-repair)
;;; vc-repair.el ends here
