;;; vc-hg-ign.el --- Specialized ignore parameters for Hg -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-hg-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-hg-ign
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
;; Specialized ignore parameters for Hg

;;; Code:

(provide 'vc-hg-ign)
(require 'vc-ign)

;; .:lst:. start hg ignore
;; --------------------------------------------------
;; |||:sec:||| Hg specialized parameters
;; --------------------------------------------------

(put 'Hg 'vc-functions nil)
(put 'HG 'vc-functions nil)

(if (fboundp 'vc-hg-find-ignore-file)
    (defalias 'vc-hg-ign-find-ignore-file 'vc-hg-find-ignore-file)
  (defun vc-hg-ign-find-ignore-file (file)
    "Return the root directory of the repository of FILE."
    (expand-file-name ".hgignore"
                      (vc-hg-root file))))

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

;;; vc-hg-ign.el ends here
