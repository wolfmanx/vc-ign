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

(defun vc-default-ign-find-ignore-file (backend file)
  "Return the ignore file for BACKEND based on FILE."
  (vc-call-backend backend 'find-ignore-file file))

;; .:lst:. end default

(provide 'vc-default-ign)
;;; vc-default-ign.el ends here
