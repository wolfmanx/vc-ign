;;; vc-svn-ign.el --- Specialized ignore parameters for SVN -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-svn-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-svn-ign
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
;; Specialized ignore parameters for SVN

;;; Code:

(provide 'vc-svn-ign)
(require 'vc-ign)

;; .:lst:. start repair
;; --------------------------------------------------
;; |||:sec:||| REPAIR
;; --------------------------------------------------

;; GNU bug report logs - #37216 [PATCH] vc-svn-ignore sets incorrect properties for relative filenames
(defun vc-svn-ign-ignore-completion-table (directory)
  "Return the list of ignored files in DIRECTORY."
  (with-temp-buffer
    (if (= (vc-svn-command t t nil "propget" "svn:ignore" (expand-file-name directory)) 0)
        (split-string (buffer-string) "\n"))))

;; .:lst:. end repair
;; .:lst:. start svn ignore
;; --------------------------------------------------
;; |||:sec:||| SVN specialized parameters
;; --------------------------------------------------

(put 'SVN 'vc-functions nil)

(if (fboundp 'vc-svn-find-ignore-file)
    (defalias 'vc-svn-ign-find-ignore-file 'vc-svn-find-ignore-file)
  (defun vc-svn-ign-find-ignore-file (file)
    "Return the virtual ignore file for FILE."
    (expand-file-name ".svnignore" (if file (file-name-directory file)))))

(defvar vc-svn-ign-ignore-param-glob
  '(:escape: vc-ign-glob-escape :anchor: "" :trailer: "" :dir-trailer: "")
  "Ignore parameters for SVN unanchored glob wildcards.")

(defun vc-svn-ign-ignore-param (&optional _ignore-file)
  "Appropriate SVN ignore parameters for IGNORE-FILE."
  vc-svn-ign-ignore-param-glob)

(defun vc-svn-ign-modify-ignore-specs (pattern ignore-file remove)
  "Implements ‘vc-default-ign-modify-ignore-specs’ for SVN.

PATTERN is the string to be added to the ignore specifications of
the IGNORE-FILE's directory, unless REMOVE is non-nil, in which
case PATTERN is removed."
  (let* ((directory (file-name-directory ignore-file))
         (ignores (vc-svn-ign-ignore-completion-table directory))
         (ignores (if remove
                      (delete pattern ignores)
                    (push pattern ignores))))
    (vc-svn-command nil 0 nil nil "propset" "svn:ignore"
                    (mapconcat #'identity ignores "\n")
                    directory)))

;; .:lst:. end svn ignore

;;; vc-svn-ign.el ends here
