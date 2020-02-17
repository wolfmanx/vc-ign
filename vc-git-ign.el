;;; vc-git-ign.el --- Specialized ignore parameters for Git -*- lexical-binding: t -*-
;;
;; usage: (require 'vc-git-ign)
:end: ;; script-help

;; Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;;
;; Version: 1.0.0
;; Keywords: cvs src svn bzr git hg mtn vc
;; Author: Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
;; URL: http://github.com/wolfmanx/vc-git-ign
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
;; Specialized ignore parameters for Git

;;; Code:

(provide 'vc-git-ign)
(require 'vc-ign)

;; .:lst:. start git ignore
;; --------------------------------------------------
;; |||:sec:||| Git specialized parameters
;; --------------------------------------------------

(put 'Git 'vc-functions nil)
(put 'GIT 'vc-functions nil)

(defun vc-git-ign-find-ignore-file (file)
  "Return the git ignore file that controls FILE."
  (expand-file-name ".gitignore" (vc-git-root file)))
(if (fboundp 'vc-git-find-ignore-file)
    (defalias 'vc-git-ign-find-ignore-file 'vc-git-find-ignore-file))

(defun vc-git-ign-ignore-param (&optional _ignore-file)
  "Appropriate Git ignore parameters for IGNORE-FILE."
  vc-ign-ignore-param-glob-anchored)

;; .:lst:. end git ignore

;;; vc-git-ign.el ends here
