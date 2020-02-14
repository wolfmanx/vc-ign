# -*- makefile -*-

top_srcdir = .
srcdir = .

EXTRA_DIST = README.md

CLEANFILES = README.md

DISTFILES =
DISTFILES += Makefile
DISTFILES += $(EXTRA_DIST)

default: all

all: $(DISTFILES) all-local

all-local:

clean:
	test -z '$(CLEANFILES)' || rm -rf $(CLEANFILES)

install: all

dist:

tags-rc:
	gen_tags.sh --template
tags:
	gen_tags.sh --force

README.md: README.txt
	( cd doc && make all-local )
	SNIPS_PATH="doc:${SNIPS_PATH}" snr --key doc_md --key output_singlehtml --key doc_dir --value doc --mode rst "$$( pwd )/$<" \
	| sed '/^[.][.] \(include\|contents\)::/d;s,@doc_dir_prefix@,doc/,g;s,@, @ ,g;s,^[.][.] \(code-block\|uml\)::,.. code::,;s,:rem:`||*:sec:||*`\\ ,,;s,:\(mod\|mod\):`\([^`]*\)`,**\2**,g;s,:\(attr\|data\|class\|meth\|func\|defun\):`\([^`]*\)`,*\2*,g;s,:\(code\|samp\|elisp\|shx\|file\|kbd\):`\([^`]*\)`,``\2``,g;s,:\(term\|term\):`\([^`]*\)`,`\2`,g' \
	| pandoc --email-obfuscation=none --from rst --to markdown_github --output - - \
	| sed '/<colgroup>/,/<\/colgroup>/d;s, @ ,@,g' >$@ || rm -f $@
	test -s $@ || ( rm -f $@; exit 1; )

# |:here:|
# :ide-menu: Emacs IDE Main Menu - Buffer @BUFFER@
# . M-x `eIDE-menu' ()(eIDE-menu "z")

# :ide: COMPILE: tags
# . (let ((args "tags")) (compile (concat "make -k " args)))

# :ide: COMPILE: dist
# . (let ((args "dist")) (compile (concat "make -k " args)))

# :ide: COMPILE: install
# . (let ((args "install")) (compile (concat "make -k " args)))

# :ide: COMPILE: clean
# . (let ((args "clean")) (compile (concat "make -k " args)))

# :ide: COMPILE: Standard
# . (let ((args "")) (compile (concat "make -k " args)))

# :ide: COMPILE: clean all
# . (let ((args "clean all")) (compile (concat "make -k " args)))

#
# Local Variables:
# mode: makefile
# snip-mode: makefile-gmake
# truncate-lines: t
# End:
