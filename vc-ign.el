;;; vc-ign.el --- Manage ignore files with VC -*- lexical-binding: t; -*-
;;
;; usage: (require 'vc-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc ignore
;; Author: Wolfgang Scherer <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-ign
;; Package-Requires: ((emacs "24"))

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
;; Load `vc-ign.el` to augment Emacs package ‘vc’ with VC ignore
;; facilities.
;;
;; - The keyboard shortcuts are bound to prefix `z` in *vc-dir-mode*
;;   and `C-x v z` in other modes.  The prefix can be customized with
;;   *vc-ign-prefix*.
;,
;; - Press `z z` in ‘vc-dir-mode’ or `C-x v z z` in ‘dired-mode’ to
;;   ignore marked files.  In other modes, a file is read from the
;;   minibuffer.  With a prefix argument, the files are removed from
;;   the ignore file.
;;
;; - Press `z p` in ‘vc-dir-mode’ or `C-x v z p` in ‘dired-mode’ for a
;;   prompt with the current file as properly quoted pattern.  In other
;;   modes, a pattern is read from the minibuffer.  With a prefix
;;   argument, the pattern is removed from the ignore file

;; - Press `z w` in ‘vc-dir-mode’ or `C-x v z w` in other modes to
;;   push the marked file names relatve to the repository root onto
;;   the ‘kill-ring’.  With a prefix argument, escape and anchor the
;;   file names.  The file names are concatenated with a newline.

;;; Code:

(eval-and-compile
  (dolist (pkg '(dired vc vc-hooks vc-dir vc-svn vc-src vc-bzr vc-git vc-hg vc-mtn))
    (condition-case nil (require pkg) (error nil))))

;; .:lst:. start package-lint
;; --------------------------------------------------
;; |||:sec:||| package-lint support
;; --------------------------------------------------

;; (insert (format "\n%S" (rx string-start (or "vc-"(or "default" "CVS" "SVN" "SRC" "Bzr" "Git" "Hg" "Mtn") "-ign"))))

(defvar vc-ign-package-lint--sane-prefixes
  "\\`\\(?:vc-\\|\\(?:Bzr\\|CVS\\|Git\\|Hg\\|Mtn\\|S\\(?:RC\\|VN\\)\\|default\\)\\|-ign\\)"
  "Sane ‘vc-’ backend prefixes for package-lint.")

(defun vc-ign-do-package-lint-current-buffer ()
  "Dummy replacement for ‘package-lint-current-buffer’.")
(if (fboundp 'package-lint-current-buffer)
    (defalias 'vc-ign-do-package-lint-current-buffer
      'package-lint-current-buffer))

(defun vc-ign-package-lint-current-buffer ()
  "Display lint errors and warnings for the current buffer.

The variable ‘package-lint--sane-prefixes’ is extended with
‘vc-ign-package-lint--sane-prefixes’ before calling
‘package-lint-current-buffer’.

Since ‘vc-call-backend’ searches for backend functions with a
hardcoded prefix of ‘vc-’, the backend functions here cannot be
named with a prefix of ‘vc-ign-' + ‘backend’.

The regular expression ‘vc-ign-package-lint--sane-prefixes’
defines backend prefixes for all supported backends, constructed
from:

  ‘vc-’ ‘backend’ ‘-ign’

Therefore, the linter still reports other functions, that do not
belong to this package."
  (interactive)
  (let* ((package-lint--sane-prefixes
          (apply #'concat
                 (delq nil
                       (append
                        (and (boundp 'package-lint--sane-prefixes)
                             (list package-lint--sane-prefixes "\\|"))
                        (list vc-ign-package-lint--sane-prefixes)))))
         (_unused package-lint--sane-prefixes))
    (vc-ign-do-package-lint-current-buffer)))

;; .:lst:. end package-lint
;; .:lst:. start backport
;; --------------------------------------------------
;; |||:sec:||| BACKPORT
;; --------------------------------------------------

;;; Compatibility
;; vc-call-backend             22
;; vc-deduce-fileset           22
;; vc-dir-current-file         23
;; vc-dir-menu-map             23
;; vc-dir-mode-map             23
;; vc-dir-move-to-goal-column  23
;; vc-dir-resynch-file         22
;; vc-dired-deduce-fileset     22
;; vc-menu-map                 22
;; vc-mtn-root                 22
;; vc-prefix-map               22
;; vc-responsible-backend      22
;; vc-svn-command              22
;; vc--read-lines              22

(eval-and-compile
  (if (fboundp 'string-match-p)
      (defalias 'vc-ign-string-match-p 'string-match-p)
    (defsubst vc-ign-string-match-p (regexp string &optional start)
      "Same as `string-match' except this function does not change the match data."
      (save-match-data
        (string-match regexp string start))))

  (if (fboundp 'cl-delete-if)
      (defalias 'vc-ign-delete-if 'cl-delete-if)
    (if (fboundp 'delete-if)
        (defalias 'vc-ign-delete-if 'delete-if)
      (defun vc-ign-delete-if (predicate seq)
        (delq nil (mapcar (lambda (s) (and (funcall predicate s) s)) seq)))))

  (if (fboundp 'pcase)
      (defalias 'vc-ign-case 'pcase)
    (if (fboundp 'cl-case)
        (defalias 'vc-ign-case 'cl-case)
      (defalias 'vc-ign-case 'case)))

  (if (fboundp 'bindings--define-key)
      (defalias 'vc-ign-bindings--define-key 'bindings--define-key)
    (defun vc-ign-bindings--define-key (map key item)
      "Define KEY in keymap MAP according to ITEM from a menu.
This is like `define-key', but it takes the definition from the
specified menu item, and makes pure copies of as much as possible
of the menu's data."
      (declare (indent 2))
      (define-key map key
        (cond
         ((not (consp item)) item)     ;Not sure that could be other than a symbol.
         ;; Keymaps can't be made pure otherwise users can't remove/add elements
         ;; from/to them any more.
         ((keymapp item) item)
         ((stringp (car item))
          (if (keymapp (cdr item))
              (cons (purecopy (car item)) (cdr item))
            (purecopy item)))
         ((eq 'menu-item (car item))
          (if (keymapp (nth 2 item))
              `(menu-item ,(purecopy (nth 1 item)) ,(nth 2 item)
                          ,@(purecopy (nthcdr 3 item)))
            (purecopy item)))
         (t (message "non-menu-item: %S" item) item)))))

  (if (fboundp 'vc-deduce-fileset)
      (defalias 'vc-ign-vc-deduce-fileset 'vc-deduce-fileset)
    (defun vc-ign-vc-deduce-fileset (&optional _observer _allow-unregistered
                                               _state-model-only-files)
      (when (derived-mode-p 'dired-mode)
        (vc-ign-vc-dired-deduce-fileset))))

  (if (fboundp 'vc-dired-deduce-fileset)
      (defalias 'vc-ign-vc-dired-deduce-fileset 'vc-dired-deduce-fileset)
    (defun vc-ign-vc-dired-deduce-fileset ()
      (list (vc-responsible-backend default-directory)
            (dired-map-over-marks (dired-get-filename nil t) nil))))

  (if (fboundp 'vc-dir-resynch-file)
      (defalias 'vc-ign-vc-dir-resynch-file 'vc-dir-resynch-file)
    (defun vc-ign-vc-dir-resynch-file (&rest _args)))

  (if (fboundp 'vc--read-lines)
      (defalias 'vc-ign--read-lines 'vc--read-lines)
    (defun vc-ign--read-lines (file)
      "Return a list of lines of FILE."
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t)))))

;; .:lst:. end backport
;; .:lst:. start repair
;; --------------------------------------------------
;; |||:sec:||| REPAIR
;; --------------------------------------------------

;; GNU bug report logs - #37185 24.5.1: vc--add-line, vc--remove-regexp are sub-optimal
;; Subroutine for `vc-default-ignore'
(defun vc-ign--add-line (string file)
  "Add STRING as a line to FILE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote string) "$") nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert string "\n")
      (save-buffer))))

(defun vc-ign--remove-regexp (regexp file)
  "Remove all matching for REGEXP in FILE."
  (if (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match ""))
        (save-buffer))))

;; .:lst:. end repair
;; .:lst:. start ui
;; --------------------------------------------------
;; |||:sec:||| User Interface
;; --------------------------------------------------

(defun vc-ign-ignore-file (file &optional directory remove prompt)
  "Ignore FILE under VCS of DIRECTORY.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend.

If FILE is nil, ‘vc-ign-ignore-fileset’ is called.

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

PROMPT is passed on to ‘vc-ign-ignore-fileset’.  When called
interatively, PROMPT is set to t."
  (interactive
   (list
    (unless (or (derived-mode-p 'vc-dir-mode)
                (derived-mode-p 'dired-mode))
      (read-file-name
       (concat "File to "
               (if (not current-prefix-arg) "ignore" "remove") ": ")))
    nil current-prefix-arg (derived-mode-p 'dired-mode)))
  (if file
      (vc-ign-ignore file directory remove t)
    (vc-ign-ignore-fileset nil remove prompt)))

(defun vc-ign-ignore-pattern  (pattern &optional directory remove backend)
  "Ignore PATTERN under VCS of DIRECTORY.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend unless BACKEND is specified.

PATTERN is an expression following the rules of the backend
pattern syntax, matching the files to be ignored.  When REMOVE is
non-nil, remove PATTERN from the list of ignored files.

When called interactively, prompt for a PATTERN to ignore, unless
a prefix argument is given, in which case prompt for a PATTERN to
remove.  The completion collection contains the currently defined
patterns from the ignore file."
  (interactive
   (let* ((dir default-directory)
          (backend (or (vc-responsible-backend dir)
                       (error "Unknown backend")))
          (is-dired-mode (derived-mode-p 'dired-mode))
          (is-vc-dir-mode (derived-mode-p 'vc-dir-mode))
          (ignore-param
           (nthcdr 4 (vc-call-backend
                      backend 'ign-get-ignore-file-and-pattern
                      (if (or is-dired-mode is-vc-dir-mode)
                          (car (cadr (vc-ign-deduce-current-file t))) dir)
                      dir t nil)))
          (default-pattern (cadr ignore-param))
          (ignore-file (nth 2 ignore-param))
          (ignore-dir (file-name-directory ignore-file))
          (ignore-completion-table
           (delq nil (append
                      (list (and default-pattern
                                 (not (string= default-pattern ""))
                                 "")
                            (car ignore-param))
                      (vc-call-backend
                       backend 'ign-ignore-completion-table ignore-dir))))
          (remove current-prefix-arg))
     (list
      (completing-read
       (format "%s pattern verbatim %s %s: "
               (if remove "Remove" "Add")
               (if remove "from" "to")
               (file-relative-name ignore-file dir))
       ignore-completion-table nil nil
       default-pattern)
      ignore-dir remove backend)))
  (if backend
      (vc-call-backend backend 'ign-ignore pattern directory remove nil)
    (vc-ign-ignore pattern directory remove nil)))

(defun vc-ign-file-root-relative-name
    (files &optional directory backend to-kill-ring escape)
  "Get file names for FILES relative to root of VC.

DIRECTORY defaults to `default-directory' and is used to
determine the responsible VC backend, unless BACKEND is not-nil.

The files are concatenated with a newline.

When called interactively, or if TO-KILL-RING is non-nil, the
result is placed on the ‘kill-ring’.

If ESCAPE is non-nil (prefix, if interactive), the filenames are
escaped and anchored for BACKEND."
  (interactive
   (let* ((bf (vc-ign-vc-deduce-fileset))
          (backend (or (car bf) 'RCS))
          (files (cadr bf)))
     (list files nil backend t current-prefix-arg)))
  (let* ((directory (or directory default-directory))
         (backend
          (or backend
              (let ((default-directory directory))
                (vc-deduce-backend))
              'RCS))
         (indx (if escape 2 1))
         (relative-names
          (mapconcat
           #'(lambda (file)
               (nth indx
                    (vc-call-backend
                     backend 'ign-get-ignore-file-and-pattern
                     file directory t nil)))
           files "\n")))
    (when to-kill-ring (kill-new relative-names))
    relative-names))

;; .:lst:. end ui
;; .:lst:. start frontend
;; --------------------------------------------------
;; |||:sec:||| Frontend
;; --------------------------------------------------

(defun vc-ign-ignore (pattern-or-file &optional directory remove is-file)
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
  (setq directory (or directory default-directory))
  (vc-call-backend (or (vc-responsible-backend directory)
                       (error "Unknown backend"))
                   'ign-ignore pattern-or-file directory remove is-file))

(defvar vc-ign--ignore-fileset-po
  '(("Remove" "Removing" "removed from ignored files" " from ignored files")
    ("Ignore" "Ignoring" "ignored" ""))
  "Alternate message strings for ‘vc-ign-ignore-fileset’.")

(defun vc-ign-ignore-fileset (&optional fileset remove prompt)
  "Ignore file set under a version control system..

If FILESET is not given, it is deduced with
‘vc-ign-vc-deduce-fileset’.

When REMOVE is non-nil, remove the files from the list of ignored
files.

If PROMPT is non-nil, confirm the operation.  If the confirmation
is negative, do not perform the ignore operation."
  (let* ((fileset-arg (or fileset (vc-ign-vc-deduce-fileset t t)))
         (backend (car fileset-arg))
         (files (delq nil (nth 1 fileset-arg)))
         (msg-strings (if remove
                          (car vc-ign--ignore-fileset-po)
                        (cadr vc-ign--ignore-fileset-po)))
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
                              (nth 3 msg-strings))))))
      (setq msg (concat (message "%s %s%s... " (nth 1 msg-strings) files
                                 (nth 3 msg-strings)) "done"))
      (mapc
       (lambda (file)
         (vc-call-backend backend 'ign-ignore file nil remove t)
         (vc-ign-vc-dir-resynch-file file))
       files))
    (when (derived-mode-p 'vc-dir-mode)
      (vc-dir-move-to-goal-column))
    (message msg)))

;; .:lst:. end frontend
;; .:lst:. start generic ignore
;; --------------------------------------------------
;; |||:sec:||| Generic ignore parameters
;; --------------------------------------------------

(defvar vc-ign-ignore-param-none
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

(defvar vc-ign-ignore-param-glob
  '(:escape: vc-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for unanchored glob wildcards.")

(defvar vc-ign-ignore-param-glob-anchored
  '(:escape: vc-ign-glob-escape :anchor: "/" :trailer: "" :dir-trailer: "/")
  "Ignore parameters for anchored glob wildcards.")

(defvar vc-ign-ignore-param-regexp
  '(:escape: regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for anchored regular expressions.")

(defun vc-ign-glob-escape (string)
  "Escape special glob characters in STRING."
  (if (vc-ign-string-match-p "[\\?*[]" string)
      (mapconcat (lambda (c)
                   (or (vc-ign-case c
                         (?\\ "\\\\")
                         (?? "\\?")
                         (?* "\\*")
                         (?\[ "\\["))
                       (char-to-string c)))
                 string "")
    string))
;; (vc-ign-glob-escape "full[glo]?\\b*")

;; optimized code Python >= v3.7
;; # SPECIAL_CHARS
;; # closing ')', '}' and ']'
;; # '-' (a range in character set)
;; # '&', '~', (extended character set operations)
;; # '#' (comment) and WHITESPACE (ignored) in verbose mode
;; _special_chars_map = {i: '\\' + chr(i) for i in b'()[]{}?*+-|^$\\.&~# \t\n\r\v\f'}

(defvar vc-ign--py-regexp-special-chars
  (mapcar
   (function
    (lambda (c)
      (cons c (concat "\\" (char-to-string c)))))
   "()[]{}?*+-|^$\\.&~# \t\n\r\v\f")
  "Characters that have special meaning in Python regular expressions.")
;; (cdr (assq ?/ vc-ign--py-regexp-special-chars))
;; (cdr (assq ?\( vc-ign--py-regexp-special-chars))

(defun vc-ign-py-regexp-quote (string)
  "Python regexp to match exactly STRING and nothing else.
Ported from Python v3.7"
  (mapconcat
   (function
    (lambda (c)
      (or (cdr (assq c vc-ign--py-regexp-special-chars))
          (char-to-string c))))
   string ""))
;; (insert (format " ;; %S" (vc-ign-py-regexp-quote "abc+.?.\\g'\"hi\030|()"))) ;; "abc\\+\\.\\?\\.\\\\g'\"hi\\|\\(\\)"
;; (insert (format " ;; %S" (regexp-quote       "abc+.?.\\g'\"hi\030|()"))) ;; "abc\\+\\.\\?\\.\\\\g'\"hi|()"

;; .:lst:. end generic ignore
;; .:lst:. start tools
;; --------------------------------------------------
;; |||:sec:||| Tools
;; --------------------------------------------------

(defun vc-ign-deduce-current-file (&optional not-buffer)
  "Deduce a the current files and a backend to which to apply an operation.
If NOT-BUFFER is not nil, do not use buffer file name as candidate."
  (list (vc-deduce-backend)
        (cond
         ((derived-mode-p 'vc-dir-mode) (list (vc-dir-current-file)))
         ((derived-mode-p 'dired-mode) (dired-get-marked-files nil t))
         (t (or (and (not not-buffer) (buffer-file-name))
                default-directory)))))
;; (let ((d default-directory))  (equal (vc-default-ign-get-ignore-file-and-pattern 'Git nil d t) (vc-default-ign-get-ignore-file-and-pattern 'Git d d t))) => t

(defun vc-ign-expand-file-name (file &optional directory)
  "Call ‘expand-file-name’ with normalized FILE and DIRECTORY.

Avoids removing the final slash of directories from the
expansion, if FILE does not have a trailing slash."
  (if (or (string= file "")
          (string= file ".")
          (string= file "..")
          (and (>= (length file) 2)
               (or (string= (substring file -2) "/.")
                   (and (>= (length file) 3)
                        (string= (substring file -3) "/..")))))
      (setq file (file-name-as-directory file)))
  (setq file (expand-file-name file directory))
  (if (and (not (vc-ign-has-final-slash file))
           (file-directory-p file))
      (setq file (file-name-as-directory file)))
  file)

;; (insert (format " ;; => %S" (vc-ign-expand-file-name "xx/"    "/some/dir/"))) ;; => "/some/dir/xx/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name ""       "/some/dir/"))) ;; => "/some/dir/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name "."      "/some/dir/"))) ;; => "/some/dir/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name ".."     "/some/dir/"))) ;; => "/some/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name "zz/./"  "/some/dir/"))) ;; => "/some/dir/zz/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name "zz/."   "/some/dir/"))) ;; => "/some/dir/zz/"
;; (insert (format " ;; => %S" (vc-ign-expand-file-name "zz/.."  "/some/dir/"))) ;; => "/some/dir/"

;; (insert (format " ;; => %S" (vc-ign-expand-file-name "/usr/local"  "/some/dir/"))) ;; => "/usr/local/"

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

(defun vc-ign-file-name-directory (file &optional dir dir-as-file)
  "Get directory name for FILE.
FILE is expanded against DIR.  If FILE is a directory and DIR-AS-FILE
is non-nil, its parent directory is returned."
  (and file
       (let* ((path (expand-file-name file dir)))
         (file-name-directory
          (if dir-as-file
              (directory-file-name path)
            path)))))

(defun vc-ign-file-relative-name (file &optional dir dir-is-empty)
  "Get relative file name for FILE against DIR.
If FILE is a directory and DIR-IS-EMPTY is non-nil, nil is returned.
Otherwise, if FILE is a directory, the final slash is removed."
  (and (not (and dir-is-empty (file-directory-p file)))
       (directory-file-name (file-relative-name file dir))))

(defun vc-ign-has-final-slash (s)
  "Return index of final slash in string S or nil."
  (let ((l (1- (length s))))
    (and (> l 0) (eq (aref s l) ?/) l)))

(defun vc-ign-escape-pattern (pattern ignore-param is-dir)
  "Escape and anchor PATTERN using IGNORE-PARAM.
If IS-DIR is not nil, anchor the pattern as directory."
  (concat
   (plist-get ignore-param :anchor:)
   (funcall (or (plist-get ignore-param :escape:) #'identity) pattern)
   (or (and is-dir (plist-get ignore-param :dir-trailer:))
       (plist-get ignore-param :trailer:))))

;; .:lst:. end tools

;; \|||:here:||||:todo:|

;; .:lst:. end mtn ignore
;; .:lst:. start integration
;; --------------------------------------------------
;; |||:sec:||| Integration
;; --------------------------------------------------

(defgroup vc-ign nil
  "Customization options for VC ignore feature."
  :group 'vc
  :version "24")

(defcustom vc-ign-prefix
  [?z]
  "Key-sequence for binding shortcuts.
The shortcuts are bund to this key sequence in variables
‘vc-prefix-map’ and ‘vc-dir-mode-map’."
  :type 'key-sequence
  :group 'vc-ign)

(defvar vc-ign-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'vc-ign-ignore-file)
    (define-key map "p" 'vc-ign-ignore-pattern)
    (define-key map "w" 'vc-ign-file-root-relative-name)
    (define-key map "z" 'vc-ign-ignore-file)
    map))
(fset 'vc-ign-prefix-map vc-ign-prefix-map)

(defvar vc-ign-menu-map
  (let ((map (make-sparse-keymap)))
    (vc-ign-bindings--define-key map [vc-ign-ignore-pattern]
      '(menu-item "VC Ignore Pattern..." vc-ign-ignore-pattern
                  :help "Ignore a pattern under current version control system"))
    (vc-ign-bindings--define-key map [vc-ign-ignore-file]
      '(menu-item "VC Ignore File..." vc-ign-ignore-file
                  :help "Ignore a file under current version control system"))
    map))

(define-key vc-prefix-map vc-ign-prefix 'vc-ign-prefix-map)
(vc-ign-bindings--define-key vc-menu-map [vc-ign-ignore] (cons "VC Ignore" vc-ign-menu-map))

(when (boundp 'vc-dir-mode-map)
  (define-key vc-dir-mode-map  vc-ign-prefix 'vc-ign-prefix-map)
  (vc-ign-bindings--define-key vc-dir-menu-map [vc-ign-ignore] (cons "VC Ignore" vc-ign-menu-map)))

;; .:lst:. end integration

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

;;; :ide: CMD: (compile "EMACS='emacs24' cask build" nil)
;;; . (compile "EMACS='emacs24' cask build" nil)

;;; :ide: CMD: (compile "cask build" nil)
;;; . (compile "cask build" nil)

;; :ide: +-#+
;; . Compile ()

;;
;; Local Variables:
;; mode: emacs-lisp
;; truncate-lines: t
;; End:

(provide 'vc-ign)

(eval-and-compile
  (let ((load-path
         (cons (file-name-directory
                (or load-file-name (buffer-file-name) "."))
               load-path)))
    (dolist (pkg '(vc-default-ign vc-cvs-ign vc-svn-ign vc-src-ign vc-bzr-ign vc-git-ign vc-hg-ign vc-mtn-ign))
      (condition-case nil
          (require pkg)
        (error nil)))))

;;; vc-ign.el ends here
