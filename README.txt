.. -*- coding: utf-8 -*-
.. \||<-snip->|| start
.. \||<-snap->|| include ^doc_defs.snip$
.. \||<-snap->|| part_top
.. Copyright (C) 2020, Wolfgang Scherer, <Wolfgang.Scherer at gmx.de>
..
.. This file is part of VC Ignore.
..
.. Permission is granted to copy, distribute and/or modify this document
.. under the terms of the GNU Free Documentation License, Version 1.3
.. or any later version published by the Free Software Foundation;
.. with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
.. A copy of the license is included in the section entitled "GNU
.. Free Documentation License".

.. inline comments (with du_comment_role)
.. role:: rem(comment)
.. role:: html(raw)
   :format: html
.. role:: shx(code)
   :language: sh
.. role:: elisp(code)
   :language: elisp
.. role:: defun(func)

.. \||<-snap->|| part_top
.. \||<-snap->|| part_title

####################################################
:rem:`|||:sec:|||`\ vc-ign - VC Ignore Emacs Package
####################################################
.. \||<-snap->|| part_title
.. \||<-snap->|| part_contents
.. \||<-snap->|| not_doc_md

The `full HTML document <@doc_dir@/_build/html/index.html>`_ is available in
the `Sphinx HTML build directory <@doc_dir@/_build/html/>`_
.. \||<-snap->|| not_doc_md

:Author: `Wolfgang Scherer`_

.. \||<-snap->|| part_contents
.. \||<-snap->|| part_abstract
.. >>CODD See `the components of a doctoral dissertation and their order <http://site.uit.no/english/writing-style/bookstructure/>`_
.. >>CODD Dedication
.. >>CODD Epigraph
.. >>CODD Abstract
.. \||<-snap->|| subst seclevel section
.. \||<-snap->|| not_doc_standalone
.. \||<-snap->|| subst seclevel chapter
.. \||<-snap->|| not_doc_chapter

.. raw:: html

   <p>
   There is a  <a href="../latex/VCIgnore.pdf">PDF version of this document</a> available.
   </p>
.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_standalone

.. \||<-snap->|| not_doc_md
.. raw:: latex

   \iffalse

.. \||<-snap->|| not_doc_md
.. rubric:: Quickstart
.. \||<-snap->|| not_doc_md
.. raw:: latex

   \fi
   \providecommand{\sddlytocignore}[1]{#1\ignorespaces}
   \sddlytocignore{%
   \addcontentsline{toc}{\sdseclevel}{Quickstart}%
   \markboth{}{}%
   \phantomsection
   }%
.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_md
   \begingroup
   \renewcommand{\abstractname}{Quickstart}
   \pagebreak[3]
   \begin{abstract}
   \nobreak\noindent\setlength{\parindent}{0pt}%
.. \||<-snap->|| not_doc_md

- Load :file:`x-vc-repair.el` to augment Emacs package :mod:`vc` with VC
  ignore facilities.

- Press :kbd:`F` in :defun:`vc-dir-mode` or :kbd:`C-x v F` in
  :defun:`dired-mode` to ignore marked files.

- Press :kbd:`G` in :defun:`vc-dir-mode` or :kbd:`C-x v G` in
  :defun:`dired-mode` for a prompt with the current file as properly
  quoted pattern.

.. \|:here:|

.. \||<-snap->|| not_doc_md
.. raw:: latex

   \end{abstract}
   \endgroup

.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_chapter

.. \||<-snap->|| not_doc_md
.. raw:: latex

   \iffalse

.. \||<-snap->|| not_doc_md
.. rubric:: Abstract
.. \||<-snap->|| not_doc_md
.. raw:: latex

   \fi
   \providecommand{\sddlytocignore}[1]{#1\ignorespaces}
   \sddlytocignore{%
   \addcontentsline{toc}{\sdseclevel}{Abstract}%
   \markboth{}{}%
   \phantomsection
   }%
.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_md
   \pagebreak[3]
   \begin{abstract}
   \nobreak\noindent\setlength{\parindent}{0pt}%

.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_chapter
.. compound::

   .. \|:here:|

.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_md
.. raw:: latex

   \end{abstract}

.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_md
.. raw:: latex

   \providecommand{\sddlytoctableofcontents}{}
   \sddlytoctableofcontents

.. \||<-snap->|| not_doc_md
.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| part_abstract
.. \||<-snap->|| part_contents
.. >>CODD Contents page

.. contents::
..

.. For LaTeX output, LOF, LOT, LOCB are in the preamble (see conf.py)
.. >>CODD List of Illustrations
.. >>CODD List of Tables
.. \||<-snap->|| part_contents
.. \||<-snap->|| inc_index ^index-contents.snip$
.. \||<-snap->|| part_abstract
.. \||<-snap->|| not_doc_overview
.. \||<-snap->|| not_doc_chapter
.. >>CODD Acknowledgments
.. >>CODD Note on Transliterations
.. >>CODD List of Abbreviations

.. include:: @doc_dir_prefix@abbrevs.inc
.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_overview
.. \||<-snap->|| part_abstract
.. \||<-snap->|| inc_standalone ^standalone-header.snip$
.. \||<-snap->|| inc_index ^index-header.snip$
.. \||<-snap->|| inc_overview ^overview-header.snip$
.. \||<-snap->|| inc_chapter ^chapter-header.snip$
.. \||<-snap->|| part_body
.. >>CODD Introduction
.. \||<-snap->|| not_doc_slides

==================================================
:rem:`|||:sec:|||`\ Introduction
==================================================

.. \||<-snap->|| not_doc_slides
.. >>CODD Chapter
.. >>CODD Conclusion
.. >>CODD Appendix A

.. \|:here:|

.. >>CODD Notes
.. ==================================================
.. :rem:`|||:sec:|||`\ Footnotes
.. ==================================================
.. \||<-snap->|| not_doc_md

:html:`<hr>`
.. \||<-snap->|| not_doc_md

.. \[#]

.. \||<-snap->|| part_body
.. \||<-snap->|| part_refs
.. \||<-snap->|| not_doc_overview
.. \||<-snap->|| not_doc_chapter

.. >>CODD Reference List/Bibliography
.. ==================================================
.. :rem:`|||:sec:|||`\ Glossary
.. ==================================================

.. include:: @doc_dir_prefix@glossary.inc

.. ==================================================
.. :rem:`|||:sec:|||`\ References
.. ==================================================

.. \||<-snap->|| not_doc_md
.. raw:: latex

   \iffalse

.. \||<-snap->|| not_doc_md
.. rubric:: References
.. \||<-snap->|| not_doc_md
.. raw:: latex

   \fi
.. \||<-snap->|| not_doc_md

.. include:: @doc_dir_prefix@bibliography.inc

.. \||<-snap->|| not_doc_chapter
.. \||<-snap->|| not_doc_overview
.. \||<-snap->|| part_refs
.. \||<-snap->|| part_defs
.. include:: @doc_dir_prefix@doc_defs.inc
.. include:: @doc_dir_prefix@abbrev_defs.inc
.. \||<-snap->|| doc_standalone
.. include:: @doc_dir_prefix@doc_defs_standalone.inc
.. \||<-snap->|| doc_standalone
.. \||<-snap->|| not_doc_standalone
.. @include_directive_combined@ doc_defs_combined.inc
.. \||<-snap->|| not_doc_standalone
.. \||<-snap->|| include ^doc_defs_global.snip$
.. \||<-snap->|| part_defs
.. \||<-snap->|| inc_standalone ^standalone-footer.snip$
.. \||<-snap->|| inc_index ^index-footer.snip$
.. \||<-snap->|| inc_overview ^overview-footer.snip$
.. \||<-snap->|| inc_chapter ^chapter-footer.snip$
.. \||<-snap->|| part_bottom
.. \||<-snap->|| doc_standalone

:rem:`|||:sec:|||`\ **Copyright**

Copyright (C) 2020, `Wolfgang Scherer`_, <Wolfgang.Scherer at gmx.de>.
See the document source for conditions of use under the GNU Free
Documentation License.

.. \||<-snap->|| doc_standalone

.. _`Wolfgang Scherer`: wolfgang.scherer@gmx.de

.. \||<-snap->|| part_bottom
.. \||<-snip->|| stop

.. ==================================================
.. :rem:`|||:sec:|||`\ END
.. ==================================================

.. (progn (forward-line 1) (snip-insert "rst_t.ide-update" t t "rst") (insert "\n"))
.. 
.. :ide-menu: Emacs IDE Main Menu - Buffer @BUFFER@
.. . M-x `eIDE-menu' ()(eIDE-menu "z")

.. (progn (forward-line 1) (snip-insert "rst_ide.promote-sections" t t "rst") (insert "\n"))

.. :ide: +#-
.. . Local Commands () |:here:|

.. :ide: DELIM: SNIPPETS (ABOUT)       |q|<- SYM ->||,   ||<- SYM ->||,  @| SYM @
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (setq symbol-tag-match-rx "sn[i]p") (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons nil "@|") (cons "@" nil)))))

.. :ide: DELIM: SNIPPETS (DOC)          ||<- SYM ->||,     |: SYM :|,     ` SYM `
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "|:") (cons ":|" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (setq symbol-tag-match-rx "sn[i]p") (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons "\\(\\`\\|[^\\]\\)" "`") (cons "`" nil)))))

.. :ide: DELIM: SNIPPETS (SNIP DOC)     ||<- SYM ->||,     |: SYM :|,     @ SYM @
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "|:") (cons ":|" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (setq symbol-tag-match-rx "sn[i]p") (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons nil "@") (cons "@" nil)))))

.. :ide: DELIM: SNIPPETS (FILLME)       ||<- SYM ->||,     :: SYM ::,     @ SYM @
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "::") (cons "::" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (setq symbol-tag-match-rx "sn[i]p") (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons nil "@") (cons "@" nil)))))

.. :ide: DELIM: SNIPPETS (SUBST)        ||<- SYM ->||,      @ SYM @,      @ SYM @
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "@") (cons "@" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "||<-") (cons "->||" nil)) t) (setq symbol-tag-match-rx "sn[i]p") (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons "[^\\]" "`") (cons "`" nil)))))

.. :ide: +#-
.. . Snippet Delimiter Sets ()

.. :ide: DELIM: ReST (links)              ` SYM `_,    .. _` SYM `,      ` SYM `
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons "[^\\]" "`") (cons "`_" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil ".. _`") (cons "`:" nil)) t) (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons "\\(\\`\\|[^\\]\\)" "`") (cons "`" nil)))))

.. :ide: DELIM: STANDARD (GNU quoting)    |: SYM :|,       :: SYM ::,     ` SYM '
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "::") (cons "::" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "|:") (cons ":|" nil)) t) (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons nil "`") (cons "'" nil)))))

.. :ide: DELIM: STANDARD (ReST quoting)   |: SYM :|,       :: SYM ::,     ` SYM `
.. . (let nil (symbol-tag-normalize-delimiter (cons (cons nil "::") (cons "::" nil)) t) (symbol-tag-switch-delimiter-sets) (symbol-tag-normalize-delimiter (cons (cons nil "|:") (cons ":|" nil)) t) (setq symbol-tag-enclose-delimiter-set (symbol-tag-normalize-delimiter (cons (cons "[^\\]" "`") (cons "`" nil)))))

.. :ide: +#-
.. . Delimiter Sets ()

.. (progn (forward-line 1) (snip-insert "gen_ide.pandoc_from_rst" t t "rst") (insert "\n"))

..  :ide: PANDOC: --to latex
..  . (progn (save-buffer) (shell-command (concat "sed '/^[.][.] \\(include\\|contents\\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \\(code-block\\|uml\\)::,.. code::,;s,:rem:`||*:sec:||*`\\\\ ,,;s,:\\(mod\\|mod\\):`\\([^`]*\\)`,**\\2**,g;s,:\\(attr\\|data\\|class\\|meth\\|func\\|defun\\):`\\([^`]*\\)`,*\\2*,g;s,:\\(code\\|samp\\|elisp\\|shx\\|file\\|kbd\\):`\\([^`]*\\)`,``\\2``,g;s,:\\(term\\|term\\):`\\([^`]*\\)`,`\\2`,g' " (shell-quote-argument (file-name-nondirectory (buffer-file-name))) " | pandoc --email-obfuscation=none --from rst --to latex --standalone --output - - " " | sed '/<colgroup>/,/<\\/colgroup>/d;s, @ ,@,g'")))

..  :ide: PANDOC: --to mediawiki
..  . (progn (save-buffer) (shell-command (concat "sed '/^[.][.] \\(include\\|contents\\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \\(code-block\\|uml\\)::,.. code::,;s,:rem:`||*:sec:||*`\\\\ ,,;s,:\\(mod\\|mod\\):`\\([^`]*\\)`,**\\2**,g;s,:\\(attr\\|data\\|class\\|meth\\|func\\|defun\\):`\\([^`]*\\)`,*\\2*,g;s,:\\(code\\|samp\\|elisp\\|shx\\|file\\|kbd\\):`\\([^`]*\\)`,``\\2``,g;s,:\\(term\\|term\\):`\\([^`]*\\)`,`\\2`,g' " (shell-quote-argument (file-name-nondirectory (buffer-file-name))) " | pandoc --email-obfuscation=none --from rst --to mediawiki --output - - " " | sed '/<colgroup>/,/<\\/colgroup>/d;s, @ ,@,g'")))

..  :ide: PANDOC: --to markdown_strict
..  . (progn (save-buffer) (shell-command (concat "sed '/^[.][.] \\(include\\|contents\\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \\(code-block\\|uml\\)::,.. code::,;s,:rem:`||*:sec:||*`\\\\ ,,;s,:\\(mod\\|mod\\):`\\([^`]*\\)`,**\\2**,g;s,:\\(attr\\|data\\|class\\|meth\\|func\\|defun\\):`\\([^`]*\\)`,*\\2*,g;s,:\\(code\\|samp\\|elisp\\|shx\\|file\\|kbd\\):`\\([^`]*\\)`,``\\2``,g;s,:\\(term\\|term\\):`\\([^`]*\\)`,`\\2`,g' " (shell-quote-argument (file-name-nondirectory (buffer-file-name))) " | pandoc --email-obfuscation=none --from rst --to markdown_strict --output - - " " | sed '/<colgroup>/,/<\\/colgroup>/d;s, @ ,@,g'")))

..  :ide: PANDOC: --to markdown
..  . (progn (save-buffer) (shell-command (concat "sed '/^[.][.] \\(include\\|contents\\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \\(code-block\\|uml\\)::,.. code::,;s,:rem:`||*:sec:||*`\\\\ ,,;s,:\\(mod\\|mod\\):`\\([^`]*\\)`,**\\2**,g;s,:\\(attr\\|data\\|class\\|meth\\|func\\|defun\\):`\\([^`]*\\)`,*\\2*,g;s,:\\(code\\|samp\\|elisp\\|shx\\|file\\|kbd\\):`\\([^`]*\\)`,``\\2``,g;s,:\\(term\\|term\\):`\\([^`]*\\)`,`\\2`,g' " (shell-quote-argument (file-name-nondirectory (buffer-file-name))) " | pandoc --email-obfuscation=none --from rst --to markdown --output - - " " | sed '/<colgroup>/,/<\\/colgroup>/d;s, @ ,@,g'")))

..  :ide: PANDOC: --to markdown_github
..  . (progn (save-buffer) (shell-command (concat "sed '/^[.][.] \\(include\\|contents\\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \\(code-block\\|uml\\)::,.. code::,;s,:rem:`||*:sec:||*`\\\\ ,,;s,:\\(mod\\|mod\\):`\\([^`]*\\)`,**\\2**,g;s,:\\(attr\\|data\\|class\\|meth\\|func\\|defun\\):`\\([^`]*\\)`,*\\2*,g;s,:\\(code\\|samp\\|elisp\\|shx\\|file\\|kbd\\):`\\([^`]*\\)`,``\\2``,g;s,:\\(term\\|term\\):`\\([^`]*\\)`,`\\2`,g' " (shell-quote-argument (file-name-nondirectory (buffer-file-name))) " | pandoc --email-obfuscation=none --from rst --to markdown_github --output - - " " | sed '/<colgroup>/,/<\\/colgroup>/d;s, @ ,@,g'")))

..  :ide: -#+
..  . From reStructured Text ()

.. :ide: REGION: Run table.py --help
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command (concat "python " project-dir "/ws_rfid/pyjsmo/pyjsmo/table.py --help ")))

.. :ide: REGION: Run fmt_tables --help
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --help ")))

.. :ide: REGION: Run fmt_tables --border help
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --border help ")))

.. :ide: REGION: Run fmt_tables --border bar_double_outer region
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command-on-region (region-beginning) (region-end) (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --width 500 --border bar_double_outer ")))

.. :ide: REGION: Run fmt_tables --border csv on region
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command-on-region (region-beginning) (region-end) (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --width 500 --border csv ")))

.. :ide: REGION: Run fmt_tables --border ascii on region --headlines 1
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command-on-region (region-beginning) (region-end) (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --width 500 --border ascii --headlines 1 ")))

.. :ide: REGION: Run fmt_tables --border rest on region --headlines 1
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command-on-region (region-beginning) (region-end) (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --width 500 --border rest --headlines 1 ")))

.. :ide: REGION: Run fmt_tables --border bar on region
.. . (let* ((check-users '("ws" "sw" "wscherer" "wiedenmann")) check-dir (project-dir (progn (while check-users (setq check-dir (concat "/home/" (car check-users) "/project")) (if (file-directory-p check-dir) (setq check-users nil) (setq check-users (cdr check-users)))) check-dir))) (shell-command-on-region (region-beginning) (region-end) (concat "python " project-dir "/ws_rfid/pyjsmo/fmt_tables.py --width 500 --border bar ")))

.. :ide: +-#+
.. . ASCII Tables ()

.. :ide: COMPILE: render reST as LaTeX
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2latex.py --traceback | tee " (shell-quote-argument fn) ".tex"))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: COMPILE: render reST as MAN
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2man.py --traceback "))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: COMPILE: render reST as TXT (via MAN)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2man.py --traceback | man -l -"))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; snr " args))))

.. :ide: COMPILE: render reST as ODT --strip-comments
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2odt.py --traceback --strip-comments | cat >" (shell-quote-argument fn) ".odt "))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: COMPILE: render reST as LaTeX, compile PDF and view with xdg-open
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2latex.py --traceback | tee " (shell-quote-argument fn) ".tex && pdflatex '\\nonstopmode\\input " (shell-quote-argument fn) ".tex' && xdg-open " (shell-quote-argument fn) ".pdf"))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: COMPILE: render reST as PDF
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2pdf -e ws_docutils.raw_role >" (shell-quote-argument fn) ".pdf"))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: COMPILE: render reST as HTML
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " " (shell-quote-argument fp) " | ws_rst2html.py --traceback --cloak-email-addresses | ${SED__PROG-sed} '\n/<\\/head>/i\\\n<style type=\"text/css\">\\\nimg { max-width\: 1200px; }\\\n</style>\n' | tee " (shell-quote-argument fn) ".html "))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; cat " args))))

.. :ide: +#-
.. . Process with ws_rst2xxx ()

.. :ide: CMD: show doc-dir/_build PDF output
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (shell-command (concat "xdg-open \"$( ls " fd doc-dir "/_build/latex/*.aux | sed 's,\\.aux,.pdf,' )\"" ) nil))

.. :ide: CMD: show doc-dir/_build HTML output
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (shell-command (concat "xdg-open " fd doc-dir "/_build/html/index.html" ) nil))

.. :ide: CMD: show PDF output
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fb (file-name-sans-extension fp))) (shell-command (concat "xdg-open '" fb ".pdf'")))

.. :ide: CMD: show HTML output
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fb (file-name-sans-extension fp))) (shell-command (concat "xdg-open '" fb ".html'")))

.. :ide: +#-
.. . Show Ouput ()

.. :ide: COMPILE: clean _build directory
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (compile (concat "cd " fd " && cd " doc-dir "/ && ( make clean 2>/dev/null || rm -rf _build *.rst.auto )" ) nil))

.. :ide: #
.. . ()

.. :ide: COMPILE: Complete versions (HTML/PDF)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (compile (concat "cd " fd " && cd  " doc-dir "/ && make html && make latexpdf" ) nil))

.. :ide: COMPILE: Standalone Versions (HTML/PDF)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (compile (concat "cd " fd " && PATH=\".:$PATH\" && sphinx-readme.sh --doc-dir " doc-dir "/ --format singlehtml " fn " && PATH=\".:$PATH\" && sphinx-readme.sh --doc-dir " doc-dir "/ --format pdf " fn ) nil))

.. :ide: COMPILE: All versions (HTML/PDF)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (fd (file-name-directory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (compile (concat "cd " fd " && PATH=\".:$PATH\" && sphinx-readme.sh --doc-dir " doc-dir "/ --format singlehtml " fn " && PATH=\".:$PATH\" && sphinx-readme.sh --doc-dir " doc-dir "/ --format pdf " fn " && cd " doc-dir "/ && make html && make latexpdf" ) nil))

.. :ide: #
.. . ()

.. :ide: COMPILE: render reST as EPUB (sphinx-readme.sh)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " --format epub " (shell-quote-argument fp)))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; sphinx-readme.sh --doc-dir " doc-dir "/ " args))))

.. :ide: COMPILE: render reST as PDF  (sphinx-readme.sh)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " --format pdf " (shell-quote-argument fp)))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; sphinx-readme.sh --doc-dir " doc-dir "/ " args))))

.. :ide: COMPILE: render doc as HTML  (sphinx-build in doc)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let () (and (buffer-file-name) (save-buffer)) (compile (concat "cd " doc-dir "/ && make html"))))

.. :ide: COMPILE: render reST as HTML (sphinx-readme.sh)
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp)) (doc-dir (or (and (boundp 'sphinx-doc-doc-dir) sphinx-doc-doc-dir) "doc"))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " --format singlehtml " (shell-quote-argument fp)))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; sphinx-readme.sh --doc-dir " doc-dir "/ " args))))

.. :ide: +#-
.. . Process with sphinx-readme.sh ()

.. :ide: COMPILE: render reST as pseudoXML
.. . (let* ((fp (or (buffer-file-name) (concat default-directory (and (boundp 'sphinx-doc-master) sphinx-doc-master) "README.txt"))) (fn (file-name-nondirectory fp))) (save-match-data (if (string-match-t "[.][^.]*$" fn) (setq fn (replace-match "" nil t fn)))) (let ((args (concat " --traceback " (shell-quote-argument fp) " 2>&1 #| tee " (shell-quote-argument fn) ".pxml"))) (and (buffer-file-name) (save-buffer)) (compile (concat "PATH=\".:$PATH\"; ws_rst2pseudoxml.py " args))))

.. :ide: +#-
.. . Process ()

.. :ide: QUO: ~~ Subsubsection ~~
.. . (insert "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\:rem\:`|\:sec\:|`\\ ::fillme\::\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" )

.. :ide: QUO: -- Subsection --
.. . (insert "--------------------------------------------------\n\:rem\:`||\:sec\:||`\\ ::fillme\::\n--------------------------------------------------\n" )

.. :ide: QUO: == Section ==
.. . (insert "==================================================\n\:rem\:`|||\:sec\:|||`\\ ::fillme\::\n==================================================\n" )

.. :ide: +#-
.. . Sections ()

.. :ide: OCCUR-OUTLINE:  `|||: sec :|||' + ^.. + command comments
.. . (x-symbol-tag-occur-outline "sec" '("|:" ":|") (cons (cons "^" ".. ") (cons nil nil)) "\\(_`[^`\n]+`\\|\\[[^]\n]+\\]\\|[|][^|\n]+[|]\\|[^:\n]+::\\)")

.. :ide: MENU-OUTLINE:  `|||: sec :|||' + ^.. + command comments
.. . (x-eIDE-menu-outline "sec" '("|:" ":|") (cons (cons "^" ".. ") (cons nil nil)) "\\(_`[^`\n]+`\\|\\[[^]\n]+\\]\\|[|][^|\n]+[|]\\|[^:\n]+::\\)")

.. 
.. Local Variables:
.. mode: rst
.. snip-mode: rst
.. truncate-lines: t
.. symbol-tag-symbol-regexp: "[-0-9A-Za-z_#]\\([-0-9A-Za-z_. ]*[-0-9A-Za-z_]\\|\\)"
.. symbol-tag-auto-comment-mode: nil
.. symbol-tag-srx-is-safe-with-nil-delimiters: nil
.. End:
.. sphinx-doc-master: "README.txt"
.. sphinx-doc-doc-dir: "doc"
