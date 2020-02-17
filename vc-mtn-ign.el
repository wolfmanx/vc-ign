;;; vc-mtn-ign.el --- Specialized ignore parameters for Mtn -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-mtn-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-mtn-ign
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
;; Specialized ignore parameters for Mtn

;;; Code:

(provide 'vc-mtn-ign)
(require 'vc-ign)

;; .:lst:. start mtn ignore
;; --------------------------------------------------
;; |||:sec:||| Mtn specialized parameters
;; --------------------------------------------------

(put 'Mtn 'vc-functions nil)
(put 'MTN 'vc-functions nil)

(defun vc-mtn-ign-find-ignore-file (file)
  "Return the mtn ignore file that controls FILE."
  (expand-file-name ".mtn-ignore" (vc-mtn-root file)))
(if (fboundp 'vc-mtn-find-ignore-file)
    (defalias 'vc-mtn-ign-find-ignore-file 'vc-mtn-find-ignore-file))

(defvar vc-mtn-ign-ignore-param-regexp
  '(:escape: vc-ign-py-regexp-quote :anchor: "^" :trailer: "$" :dir-trailer: "/")
  "Ignore parameters for Mtn anchored regular expressions.")

(defun vc-mtn-ign-ignore-param (&optional _ignore-file)
  "Appropriate Mtn ignore parameters for IGNORE-FILE."
  vc-mtn-ign-ignore-param-regexp)

;;; vc-mtn-ign.el ends here
