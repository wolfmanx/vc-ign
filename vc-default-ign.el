;;; vc-default-ign.el --- Default implementations for VC ignore backend functions -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-default-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-default-ign
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
;; Default implementations for VC ignore backend functions

;;; Code:

(eval-and-compile
  (provide 'vc-default-ign)
  (require 'vc-ign))

;; .:lst:. start backport
;; --------------------------------------------------
;; |||:sec:||| BACKPORT
;; --------------------------------------------------

(eval-and-compile
  (defun vc-default-ign-ignore-completion-table (backend file)
    "Return the list of ignored files under BACKEND based on FILE."
    (vc-ign-delete-if
     (lambda (str)
       ;; Commented or empty lines.
       (vc-ign-string-match-p "\\`\\(?:#\\|[ \t\r\n]*\\'\\)" str))
     (let ((file (vc-call-backend backend 'ign-find-ignore-file file)))
       (and (file-exists-p file)
            (vc-ign--read-lines file))))))

;; .:lst:. end backport
;; .:lst:. start default
;; --------------------------------------------------
;; |||:sec:||| Default
;; --------------------------------------------------

(defun vc-default-ign-ignore (backend pattern-or-file &optional directory remove is-file)
  "Implements ‘vc-ign-ignore’ generically.
BACKEND is the current backend.  Parameters PATTERN-OR-FILE,
DIRECTORY, REMOVE, IS-FILE are passed on from ‘vc-ign-ignore’."
  (apply #'vc-call-backend backend 'ign-modify-ignore-specs
         (nthcdr 5 (vc-call-backend
                    backend 'ign-get-ignore-file-and-pattern
                    pattern-or-file directory is-file remove))))

(defun vc-default-ign-get-ignore-file-and-pattern (backend pattern-or-file &optional directory is-file remove)
  "Determine ignore file and pattern for BACKEND from PATTERN-OR-FILE.
Implements API of ‘vc-ign-ignore’ for PATTERN-OR-FILE, DIRECTORY and IS-FILE.
REMOVE is passed through without evaluation.
Returns a list
    \(ignore-param
      root-rel-path     root-rel-pattern     root-dir
      ign-file-rel-path ign-file-rel-pattern ignore-file
      remove)
suitable for calling ‘vc-default-ign-modify-ignore-specs’
with (nthcdr 5 result)."
  (let* ((directory (or directory default-directory))
         root-dir
         root-rel-path)
    (if (null pattern-or-file) (setq pattern-or-file ""))
    (when is-file
      (setq pattern-or-file
            (vc-ign-expand-file-name pattern-or-file directory))
      ;; apply directory-as-file-name, otherwise, if pattern-or-file was
      ;; a sub-repository, ign-find-ignore-file would return the wrong
      ;; ignore file:
      ;; (vc-cvs-ign-find-ignore-file "/re/po/dir/") => /re/po/dir/.cvsignore
      ;; (vc-cvs-ign-find-ignore-file "/re/po/dir") => /re/po/.cvsignore
      (setq root-dir (or (ignore-errors
                           (vc-call-backend backend 'root directory))
                         directory))
      (setq root-rel-path (directory-file-name
                           (file-relative-name pattern-or-file root-dir)))
      (if (not (string= pattern-or-file directory))
          (setq directory (file-name-directory
                           (directory-file-name pattern-or-file)))))

    (let* ((ignore-file
            (vc-call-backend backend 'ign-find-ignore-file directory))
           (ignore-dir (file-name-directory ignore-file))
           is-dir ignore-param)
      (if (not is-file)
          (setq ignore-param vc-ign-ignore-param-none)

        ;; prepare file pattern
        (let* ((ignore-dir-len (length ignore-dir))
               (file-len (length pattern-or-file)))
          (unless (cond
                   ((>= file-len ignore-dir-len)
                    (string= (substring pattern-or-file 0 ignore-dir-len)
                             ignore-dir))
                   ((= (1- ignore-dir-len) file-len)
                    (string= pattern-or-file
                             (substring ignore-dir 0 file-len))))
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
          (setq ignore-param
                (vc-call-backend backend 'ign-ignore-param ignore-file))))

      (list ignore-param root-rel-path
            (and root-rel-path
                 (vc-ign-escape-pattern root-rel-path ignore-param is-dir))
            root-dir pattern-or-file
            ;; (nthcdr 5 ...) can be used as args for calling
            ;; ‘vc-default-ign-modify-ignore-specs’
            (vc-ign-escape-pattern pattern-or-file ignore-param is-dir)
            ignore-file remove))))

(defun vc-default-ign-modify-ignore-specs (_backend pattern ignore-file remove)
  "Add PATTERN to IGNORE-FILE, if REMOVE is nil..
Otherwise remove PATTERN from IGNORE-FILE."
  (if remove
      (vc-ign--remove-regexp
       (concat "^" (regexp-quote pattern) "\\(\n\\|$\\)") ignore-file)
    (vc-ign--add-line pattern ignore-file)))

(defun vc-default-ign-find-ignore-file (backend file)
  "Return the ignore file for BACKEND based on FILE."
  (vc-call-backend backend 'find-ignore-file file))

;; .:lst:. end default
;; .:lst:. start generic ignore
;; --------------------------------------------------
;; |||:sec:||| Generic ignore parameters
;; --------------------------------------------------

(defun vc-default-ign-ignore-param (_backend &optional _ignore-file)
  "Default ignore parameters for IGNORE-FILE."
  vc-ign-ignore-param-glob)

;; .:lst:. end generic ignore

;;; vc-default-ign.el ends here
