;;; vc-ign.el --- Manage ignore files with VC
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
;; - Press `z z` in ‘vc-dir-mode’ or `C-x v z z` in ‘dired-mode’ to
;;   ignore marked files. In other modes, a file is read from the
;;   minibuffer. With a prefix argument, the files are removed from
;;   the ignore file.
;;
;; - Press `z p` in ‘vc-dir-mode’ or `C-x v z p` in ‘dired-mode’ for a
;;   prompt with the current file as properly quoted pattern. In other
;;   modes, a pattern is read from the minibuffer. With a prefix
;;   argument, the pattern is removed from the ignore file

;;; Code

(let ((load-path
       (cons (file-name-directory
              (or load-file-name (buffer-file-name)))
             load-path)))
  (dolist (pkg '(vc-repair vc vc-hooks vc-svn vc-mtn))
    (condition-case err
        (require pkg)
      (error (message "error: %s (ignored)" (error-message-string err))))))

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

(unless (fboundp 'string-match-p)
(defsubst string-match-p (regexp string &optional start)
  "Same as `string-match' except this function does not change the match data."
  (save-match-data
    (string-match regexp string start))))

(if (fboundp 'cl-delete-if)
    (defalias 'vc-ign-delete-if 'cl-delete-if)
  (if (fboundp 'delete-if)
      (defalias 'vc-ign-delete-if 'delete-if)
(defun vc-ign-delete-if (predicate seq)
  (delq nil (mapcar (lambda (s) (and (funcall predicate s) s)) seq)))))

(unless (fboundp 'pcase)
  (if (fboundp 'cl-case)
      (defalias 'pcase 'cl-case)
    (defalias 'pcase 'case)))

(unless (fboundp 'bindings--define-key)
(defun bindings--define-key (map key item)
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

(unless (fboundp 'vc-deduce-fileset)
(defun vc-deduce-fileset (&optional observer allow-unregistered
                                    state-model-only-files)
  (when (derived-mode-p 'dired-mode)
    (vc-dired-deduce-fileset))))

(unless (fboundp 'vc-dired-deduce-fileset)
(defun vc-dired-deduce-fileset ()
  (list (vc-responsible-backend default-directory)
        (dired-map-over-marks (dired-get-filename nil t) nil))))

(unless (fboundp 'vc-dir-resynch-file)
(defun vc-dir-resynch-file (&rest args)))

(defun vc-default-ign-ignore-completion-table (backend file)
  "Return the list of ignored files under BACKEND."
  (vc-ign-delete-if
   (lambda (str)
     ;; Commented or empty lines.
     (string-match-p "\\`\\(?:#\\|[ \t\r\n]*\\'\\)" str))
   (let ((file (vc-call-backend backend 'ign-find-ignore-file file)))
     (and (file-exists-p file)
          (vc-ign--read-lines file)))))

(defun vc-default-ign-find-ignore-file (backend file)
  "Return the ignore file for FILE."
  (vc-call-backend backend 'find-ignore-file file))

(if (fboundp 'vc--read-lines)
    (defalias 'vc-ign--read-lines 'vc--read-lines)
(defun vc-ign--read-lines (file)
  "Return a list of lines of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t))))

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

;; GNU bug report logs - #37216 [PATCH] vc-svn-ignore sets incorrect properties for relative filenames
(defun vc-svn-ign-ignore-completion-table (directory)
  "Return the list of ignored files in DIRECTORY."
  (with-temp-buffer
    (if (= (vc-svn-command t t nil "propget" "svn:ignore" (expand-file-name directory)) 0)
        (split-string (buffer-string) "\n"))))

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

PROMPT is passed on to ‘vc-ign-ignore-fileset’. When called
interatively, PROMPT is set to ‘t’."
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
determine the responsible VC backend.

PATTERN is an expression following the rules of the backend
pattern syntax, matching the files to be ignored.  When REMOVE is
non-nil, remove PATTERN from the list of ignored files.

When called interactively, prompt for a PATTERN to ignore, unless
a prefix argument is given, in which case prompt for a PATTERN to
remove. The completion collection contains the currently defined
patterns from the ignore file."
  (interactive
   (let* ((dir default-directory)
          (backend (or (vc-responsible-backend dir)
                       (error "Unknown backend")))
          (is-dired-mode (derived-mode-p 'dired-mode))
          (is-vc-dir-mode (derived-mode-p 'vc-dir-mode))
          (ignore-param
           (if (not (or is-dired-mode is-vc-dir-mode))
               (list nil nil (vc-call-backend backend 'ign-find-ignore-file dir) nil)
             (let* ((cur-file (or
                               (and is-vc-dir-mode (vc-dir-current-file))
                               (and is-dired-mode (car (dired-get-marked-files nil t)))))
                    (ip (vc-call-backend backend 'ign-get-ignore-file-and-pattern cur-file dir t nil))
                    (cur-file-rel (file-relative-name cur-file (file-name-directory (cadr ip)))))
               (cons cur-file-rel ip))))
          (default-pattern (cadr ignore-param))
          (ignore-file (nth 2 ignore-param))
          (ignore-dir (file-name-directory ignore-file))
          (ignore-completion-table
           (delq nil (append  (list (and default-pattern "") (car ignore-param))
                              (vc-call-backend backend 'ign-ignore-completion-table ignore-dir))))
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
‘vc-deduce-fileset’.

When REMOVE is non-nil, remove the files from the list of ignored
files.

If PROMPT is non-nil, confirm the operation. If the confirmation
is negative, do not perform the ignore operation."
  (let* ((fileset-arg (or fileset (vc-deduce-fileset t t)))
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

(defun vc-default-ign-ignore (backend pattern-or-file &optional directory remove is-file)
  ;; implements ‘vc-ign-ignore’ generically
  (apply #'vc-call-backend backend 'ign-modify-ignore-specs
         (vc-call-backend backend 'ign-get-ignore-file-and-pattern
                          pattern-or-file directory is-file remove)))

(defun vc-default-ign-get-ignore-file-and-pattern (backend pattern-or-file &optional directory is-file remove)
  "Determine ignore file and pattern for BACKEND from PATTERN-OR-FILE.
Implements API of ‘vc-ign-ignore’ for PATTERN-OR-FILE, DIRECTORY and IS-FILE.
REMOVE is passed through without evaluation.
Returns (pattern ignore-file remove) suitable for calling
‘vc-default-ign-modify-ignore-specs’."

  (if (null pattern-or-file) (setq pattern-or-file ""))
  (setq directory (or directory default-directory))
  (when is-file
    (setq pattern-or-file (vc-ign-expand-file-name pattern-or-file directory))
    ;; apply directory-as-file-name, otherwise, if pattern-or-file was
    ;; a sub-repository, ign-find-ignore-file would return the wrong
    ;; ignore file:
    ;; (vc-cvs-ign-find-ignore-file "/re/po/dir/") => /re/po/dir/.cvsignore
    ;; (vc-cvs-ign-find-ignore-file "/re/po/dir") => /re/po/.cvsignore
    (if (not (string= pattern-or-file directory))
        (setq directory (file-name-directory (directory-file-name pattern-or-file)))))

  (let* ((ignore-file (vc-call-backend backend 'ign-find-ignore-file directory))
         (ignore-dir (file-name-directory ignore-file))
         is-dir ignore-param pattern)
    (if (not is-file)
        (setq ignore-param vc-ign-ignore-param-none)

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
        (setq is-dir (or (vc-ign-has-final-slash pattern-or-file)
                         (file-directory-p pattern-or-file)))
        (setq pattern-or-file
              (directory-file-name
               (substring (if is-dir
                              (file-name-as-directory pattern-or-file)
                            pattern-or-file)
                          ignore-dir-len)))
        ;; (setq debug-on-next-call t) ;; |||:here:|||
        (if (string= pattern-or-file "") (setq is-dir nil))
        (setq ignore-param (vc-call-backend backend 'ign-ignore-param ignore-file))))
    (setq pattern
          (concat
           (plist-get ignore-param :anchor:)
           (funcall (or (plist-get ignore-param :escape:) #'identity)
                    pattern-or-file)
           (or (and is-dir (plist-get ignore-param :dir-trailer:))
               (plist-get ignore-param :trailer:))))
    (list pattern ignore-file remove)))

(defun vc-default-ign-modify-ignore-specs (_backend pattern ignore-file remove)
  "Add PATTERN to IGNORE-FILE, if REMOVE is nil..
Otherwise remove PATTERN from IGNORE-FILE."
  (if remove
      (vc-ign--remove-regexp
       (concat "^" (regexp-quote pattern) "\\(\n\\|$\\)") ignore-file)
    (vc-ign--add-line pattern ignore-file)))

;; .:lst:. end default
;; .:lst:. start tools
;; --------------------------------------------------
;; |||:sec:||| Tools
;; --------------------------------------------------

(defun vc-ign-expand-file-name (file &optional directory)
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
  ;"Return index of final slash in string S or nil."
  (let ((l (1- (length s))))
    (and (> l 0) (eq (aref s l) ?/) l)))

;; .:lst:. end tools
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

(defun vc-default-ign-ignore-param (_backend &optional _ignore-file)
  "Default ignore parameters for IGNORE-FILE."
  vc-ign-ignore-param-glob)

(defun vc-ign-glob-escape (string)
  "Escape special glob characters in STRING."
  (if (string-match-p "[\\?*[]" string)
      (mapconcat (lambda (c)
                   (or (pcase c
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
;; .:lst:. start cvs ignore
;; --------------------------------------------------
;; |||:sec:||| CVS specialized parameters
;; --------------------------------------------------

(put 'CVS 'vc-functions nil)

(unless (fboundp 'vc-cvs-find-ignore-file)
(defun vc-cvs-ign-find-ignore-file (file)
  "Return the ignore file for FILE."
  (expand-file-name ".cvsignore" (if file (file-name-directory file))))
(defalias 'vc-cvs-find-ignore-file 'vc-cvs-ign-find-ignore-file))

(defvar vc-cvs-ign-ignore-param-glob
  '(:escape: vc-cvs-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "/")
  "Ignore parameters for CVS partially anchored glob wildcards.")

(defun vc-cvs-ign-ignore-param (&optional _ignore-file)
  "Appropriate CVS ignore parameters for IGNORE-FILE."
  vc-cvs-ign-ignore-param-glob)

(defun vc-cvs-ign-glob-escape (string)
  "Escape special glob characters and spaces in STRING."
  (replace-regexp-in-string " " "?" (vc-ign-glob-escape string) t))

;; .:lst:. end cvs ignore
;; .:lst:. start svn ignore
;; --------------------------------------------------
;; |||:sec:||| SVN specialized parameters
;; --------------------------------------------------

(put 'SVN 'vc-functions nil)

(unless (fboundp 'vc-svn-find-ignore-file)
(defun vc-svn-ign-find-ignore-file (file)
  "Return the virtual ignore file for FILE."
  (expand-file-name ".svnignore" (if file (file-name-directory file))))
(defalias 'vc-svn-find-ignore-file 'vc-svn-ign-find-ignore-file))

(defvar vc-svn-ign-ignore-param-glob
  '(:escape: vc-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for SVN unanchored glob wildcards.")

(defun vc-svn-ign-ignore-param (&optional _ignore-file)
  "Appropriate SVN ignore parameters for IGNORE-FILE."
  vc-svn-ign-ignore-param-glob)

(defun vc-svn-ign-modify-ignore-specs (pattern ignore-file remove)
  ;; implements ‘vc-default-ign-modify-ignore-specs’ for SVN
  (let* ((directory (file-name-directory ignore-file))
         (ignores (vc-svn-ign-ignore-completion-table directory))
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

(put 'SRC 'vc-functions nil)

(unless (fboundp 'vc-src-find-ignore-file)
(defun vc-src-ign-find-ignore-file (file)
  "Return the ignore file for FILE."
  (expand-file-name ".srcignore" (if file (file-name-directory file))))
(defalias 'vc-src-find-ignore-file 'vc-src-ign-find-ignore-file))

(defun vc-src-ign-glob-escape (string)
  "Escape special glob characters in STRING."
  (if (string-match-p "[?*[]" string)
      (mapconcat (lambda (c)
                   (or (pcase c
                         (?? "[?]")
                         (?* "[*]")
                         (?\[ "[[]")
                         (?\\ "[\\]"))
                       (char-to-string c)))
                 string "")
    string))
;; (vc-src-ign-glob-escape "full[glo]?\\b*")

(defvar vc-src-ign-ignore-param-glob
  '(:escape: vc-src-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for SRC unanchored glob wildcards.")

(defun vc-src-ign-ignore-param (&optional _ignore-file)
  "Appropriate SRC ignore parameters for IGNORE-FILE."
  vc-src-ign-ignore-param-glob)

;; .:lst:. end src ignore
;; .:lst:. start bzr ignore
;; --------------------------------------------------
;; |||:sec:||| Bzr specialized parameters
;; --------------------------------------------------

(put 'Bzr 'vc-functions nil)
(put 'BZR 'vc-functions nil)

(unless (fboundp 'vc-bzr-find-ignore-file)
(defun vc-bzr-ign-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".bzrignore"
                    (vc-bzr-root file)))
(defalias 'vc-bzr-find-ignore-file 'vc-src-bzr-find-ignore-file))

(defvar vc-bzr-ign-ignore-param-regexp
  '(:escape: vc-ign-py-regexp-quote :anchor: "RE:^" :trailer: "$" :dir-trailer: "/.*")
  "Ignore parameters for Bzr anchored regular expressions.")

(defun vc-bzr-ign-ignore-param (&optional _ignore-file)
  "Appropriate Bzr ignore parameters for IGNORE-FILE."
        vc-bzr-ign-ignore-param-regexp)

;; .:lst:. end bzr ignore
;; .:lst:. start git ignore
;; --------------------------------------------------
;; |||:sec:||| Git specialized parameters
;; --------------------------------------------------

(put 'Git 'vc-functions nil)
(put 'GIT 'vc-functions nil)

(unless (fboundp 'vc-git-find-ignore-file)
(defun vc-git-ign-find-ignore-file (file)
  "Return the git ignore file that controls FILE."
  (expand-file-name ".gitignore"
                    (vc-git-root file)))
(defalias 'vc-git-find-ignore-file 'vc-git-ign-find-ignore-file))

(defun vc-git-ign-ignore-param (&optional _ignore-file)
  "Appropriate Git ignore parameters for IGNORE-FILE."
  vc-ign-ignore-param-glob-anchored)

;; .:lst:. end git ignore
;; .:lst:. start hg ignore
;; --------------------------------------------------
;; |||:sec:||| Hg specialized parameters
;; --------------------------------------------------

(put 'Hg 'vc-functions nil)
(put 'HG 'vc-functions nil)

(unless (fboundp 'vc-hg-find-ignore-file)
(defun vc-hg-ign-find-ignore-file (file)
  "Return the root directory of the repository of FILE."
  (expand-file-name ".hgignore"
                    (vc-hg-root file)))
(defalias 'vc-hg-find-ignore-file 'vc-hg-ign-find-ignore-file))

(defvar vc-hg-ign-ignore-param-regexp
  '(:escape: vc-ign-py-regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for Hg anchored regular expressions.")

(defvar vc-hg-ign-ignore-param-glob
  '(:escape: vc-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "/*")
  "Ignore parameters for Hg anchored regular expressions.")

(defun vc-hg-ign-ignore-param (&optional ignore-file)
  "Appropriate Hg ignore parameters for IGNORE-FILE."
  (let ((syntax "regexp"))
    (if (not ignore-file)
        (setq ignore-file
              (vc-call-backend 'Hg 'ign-find-ignore-file default-directory)))
    (if (file-exists-p ignore-file)
        (with-current-buffer (find-file-noselect ignore-file)
          (save-match-data
            (goto-char (point-max))
            (if (re-search-backward "^ *syntax: *\\(regexp\\|glob\\)$" nil t)
                (setq syntax (match-string 1))))))
    (if (string= syntax "regexp")
        vc-hg-ign-ignore-param-regexp
      vc-hg-ign-ignore-param-glob)))

;; .:lst:. end hg ignore
;; .:lst:. start mtn ignore
;; --------------------------------------------------
;; |||:sec:||| Mtn specialized parameters
;; --------------------------------------------------

(put 'Mtn 'vc-functions nil)
(put 'MTN 'vc-functions nil)

(unless (fboundp 'vc-mtn-find-ignore-file)
(defun vc-mtn-ign-find-ignore-file (file)
  "Return the mtn ignore file that controls FILE."
  (expand-file-name ".mtn-ignore" (vc-mtn-root file)))
(defalias 'vc-mtn-find-ignore-file 'vc-mtn-ign-find-ignore-file))

(defvar vc-mtn-ign-ignore-param-regexp
  '(:escape: vc-ign-py-regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for Mtn anchored regular expressions.")

(defun vc-mtn-ign-ignore-param (&optional _ignore-file)
  "Appropriate Mtn ignore parameters for IGNORE-FILE."
  vc-mtn-ign-ignore-param-regexp)

;; \|||:here:||||:todo:|

;; .:lst:. end mtn ignore
;; .:lst:. start integration
;; --------------------------------------------------
;; |||:sec:||| Integration
;; --------------------------------------------------

(defvar vc-ign-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'vc-ign-ignore-file)
    (define-key map "p" 'vc-ign-ignore-pattern)
    (define-key map "z" 'vc-ign-ignore-file)
    map))
(fset 'vc-ign-prefix-map vc-ign-prefix-map)

(defvar vc-ign-menu-map
  (let ((map (make-sparse-keymap)))
    (bindings--define-key map [vc-ign-ignore-pattern]
      '(menu-item "VC Ignore Pattern..." vc-ign-ignore-pattern
                  :help "Ignore a pattern under current version control system"))
    (bindings--define-key map [vc-ign-ignore-file]
      '(menu-item "VC Ignore File..." vc-ign-ignore-file
                  :help "Ignore a file under current version control system"))
    map))

(define-key vc-prefix-map "z" 'vc-ign-prefix-map)
(bindings--define-key vc-menu-map [vc-ign-ignore] (cons "VC Ignore" vc-ign-menu-map))

(when (boundp 'vc-dir-mode-map)
  (define-key vc-dir-mode-map  "z" 'vc-ign-prefix-map)
  (bindings--define-key vc-dir-menu-map [vc-ign-ignore] (cons "VC Ignore" vc-ign-menu-map)))

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

;;
;; Local Variables:
;; mode: emacs-lisp
;; truncate-lines: t
;; End:

(provide 'vc-ign)
;;; vc-ign.el ends here
