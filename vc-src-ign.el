;;; vc-src-ign.el --- Specialized ignore parameters for SRC -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-src-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-src-ign
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
;; Specialized ignore parameters for SRC

;;; Code:

(provide 'vc-src-ign)
(require 'vc-ign)

;; .:lst:. start src ignore
;; --------------------------------------------------
;; |||:sec:||| SRC specialized parameters
;; --------------------------------------------------

(put 'SRC 'vc-functions nil)

(if (fboundp 'vc-src-find-ignore-file)
    (defalias 'vc-src-ign-find-ignore-file 'vc-src-find-ignore-file)
  (defun vc-src-ign-find-ignore-file (file)
    "Return the ignore file for FILE."
    (expand-file-name ".srcignore" (if file (file-name-directory file)))))

(defun vc-src-ign-glob-escape (string)
  "Escape special glob characters in STRING."
  (if (vc-ign-string-match-p "[?*[]" string)
      (mapconcat (lambda (c)
                   (or (vc-ign-case c
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

;;; vc-src-ign.el ends here
