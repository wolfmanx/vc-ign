;;; vc-bzr-ign.el --- Specialized ignore parameters for BZR -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-bzr-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-bzr-ign
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
;; Specialized ignore parameters for BZR

;;; Code:

(provide 'vc-bzr-ign)
(require 'vc-ign)

;; .:lst:. start bzr ignore
;; --------------------------------------------------
;; |||:sec:||| Bzr specialized parameters
;; --------------------------------------------------

(put 'Bzr 'vc-functions nil)
(put 'BZR 'vc-functions nil)

(declare-function 'vc-bzr-root "vc-bzr" (file) t)

(if (fboundp 'vc-bzr-find-ignore-file)
    (defalias 'vc-bzr-ign-find-ignore-file 'vc-bzr-find-ignore-file)
  (defun vc-bzr-ign-find-ignore-file (file)
    "Return the root directory of the repository of FILE."
    (expand-file-name ".bzrignore"
                      (vc-bzr-root file))))

(defvar vc-bzr-ign-ignore-param-regexp
  '(:escape: vc-ign-py-regexp-quote :anchor: "RE:^" :trailer: "$" :dir-trailer: "/.*")
  "Ignore parameters for Bzr anchored regular expressions.")

(defun vc-bzr-ign-ignore-param (&optional _ignore-file)
  "Appropriate Bzr ignore parameters for IGNORE-FILE."
        vc-bzr-ign-ignore-param-regexp)

;; .:lst:. end bzr ignore

;;; vc-bzr-ign.el ends here
