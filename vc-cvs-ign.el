;;; vc-cvs-ign.el --- Specialized ignore parameters for CVS -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-cvs-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-cvs-ign
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
;; Specialized ignore parameters for CVS

;;; Code:

(provide 'vc-cvs-ign)
(require 'vc-ign)

;; .:lst:. start cvs ignore
;; --------------------------------------------------
;; |||:sec:||| CVS specialized parameters
;; --------------------------------------------------

(put 'CVS 'vc-functions nil)

(defun vc-cvs-ign-find-ignore-file (file)
    "Return the ignore file for FILE."
    (expand-file-name ".cvsignore" (if file (file-name-directory file))))
(if (fboundp 'vc-cvs-find-ignore-file)
    (defalias 'vc-cvs-ign-find-ignore-file 'vc-cvs-find-ignore-file))

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

;;; vc-cvs-ign.el ends here
