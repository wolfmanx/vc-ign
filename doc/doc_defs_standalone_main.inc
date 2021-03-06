.. -*- coding: utf-8 -*-
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

.. \||<-snip->|| start

.. |chapter-index|            replace:: document :file:`README`
.. |chapter-overview|         replace:: document :file:`README`

.. \||<-snip->|| stop

.. (progn (forward-line 1) (snip-insert "rst_t.ide-update" t t "rst") (insert "\n"))
.. 
.. :ide-menu: Emacs IDE Main Menu - Buffer @BUFFER@
.. . M-x `eIDE-menu' ()(eIDE-menu "z")

.. :ide: CMD: align replace :: substitutions
.. . (progn (and (buffer-file-name) (save-buffer)) (shell-command (concat "sed 's,\",\"\",;s,^,\",;s,$,\",;s/ *replace" "::/\",\"replace" "::/p;d' " (buffer-file-name) " | xlsx-dump.sh --width 0 --header 0 --mode 'text,dense' ")))

.. :ide: +#-
.. . Tools ()

.. :ide: COMPILE: latexpdf
.. . (progn (and (buffer-file-name) (save-buffer)) (compile (concat "make -k latexpdf")))

.. :ide: COMPILE: html
.. . (progn (and (buffer-file-name) (save-buffer)) (compile (concat "make -k html")))

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
