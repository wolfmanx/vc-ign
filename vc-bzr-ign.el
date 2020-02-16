;;; vc-bzr-ign.el --- ::fillme:: -*- lexical-binding: t -*-
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

;;; Code:

(eval-and-compile
  (let ((load-path (cons "/usr/local/share/emacs/site-lisp/ws" load-path)))
    (condition-case nil
        (require 'xemacs-fsf-compat)
      ('error
       (or (fboundp 'string-match-t)
           (defalias 'string-match-t 'string-match))
       (or (fboundp 'completing-read-fsf-compat)
           (defalias 'completing-read-fsf-compat 'completing-read))
       (when (< emacs-major-version 22)
         (defadvice split-string
           (before drop-omit-nulls first
                   (string &optional separators omit-nulls)
                   activate)
           ;;drop omit-nulls
           (ad-set-args 1 (list (ad-get-arg 1)))))
       (or (fboundp 'split-string-fsf-compat)
           (defalias 'split-string-fsf-compat 'split-string))
       (or (fboundp 'replace-match-fsf-compat)
           (defalias 'replace-match-fsf-compat 'replace-match))
       (or (fboundp 'read-string-fsf-compat)
           (defalias 'read-string-fsf-compat 'read-string))))))

;; --------------------------------------------------
;; |||:sec:||| MAIN
;; --------------------------------------------------

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

(provide 'vc-bzr-ign)
;;; vc-bzr-ign.el ends here
