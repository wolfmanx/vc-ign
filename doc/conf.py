# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))

# |:here:|
import sys, os

def setdefaultencoding(encoding=None, quiet=False):
    if encoding is None:
        encoding='utf-8'
    try:
        isinstance('', basestring)
        if not hasattr(sys, '_setdefaultencoding'):
            reload(sys)
            setattr(sys, '_setdefaultencoding',
                    getattr(sys, 'setdefaultencoding'))
        sys._setdefaultencoding(encoding)
    except NameError:
        # python3 already has utf-8 default encoding ;-)
        pass

setdefaultencoding('utf-8')

# adapted from http://www.daniweb.com/software-development/python/code/217214
try:
    printf = eval("print") # python 3.0 case
except SyntaxError:
    printf_dict = dict()
    try:
        exec("from __future__ import print_function\nprintf=print", printf_dict)
        printf = printf_dict["printf"] # 2.6 case
    except SyntaxError:
        def printf(*args, **kwd): # 2.4, 2.5, define our own Print function
            fout = kwd.get("file", sys.stdout)
            w = fout.write
            if args:
                w(str(args[0]))
            sep = kwd.get("sep", " ")
            for a in args[1:]:
                w(sep)
                w(str(a))
            w(kwd.get("end", "\n"))
    del printf_dict

try:
    ('{0}').format(0)
    def sformat (fmtspec, *args, **kwargs):
        return fmtspec.format(*args, **kwargs)
except AttributeError:
    try:
        import stringformat
        def sformat (fmtspec, *args, **kwargs):
            return stringformat.FormattableString(fmtspec).format(
                *args, **kwargs)
    except ImportError:
        printf('error: stringformat missing. Try `easy_install stringformat`.', file=sys.stderr)

printe = printf

dbg_fwid = globals().get('dbg_fwid', 15)

# (progn (forward-line 1) (snip-insert "py.b.isstring" t t "py") (insert "\n"))
try:
    from ws_seq_type import isstring, issequence, sequence_type, UCHAR_FMT
except ImportError:
    # (progn (forward-line 1) (snip-insert "py.f.isstring" t t "py") (insert "\n"))
    exec('''
    def isstring(obj):
        return isinstance(obj, basestring)
    '''.strip())
    try:
        isstring("")
        UCHAR_FMT = 'u"{0}u{1:04x}"'
    except NameError:
        def isstring(obj):
            return isinstance(obj, str) or isinstance(obj, bytes)
        UCHAR_FMT = '"{0}u{1:04x}"'
    # (progn (forward-line 1) (snip-insert "py.f.issequence" t t "py") (insert "\n"))
    def issequence(arg, or_dict=False, or_seq=True):
        if not isstring(arg):
            if hasattr(arg, 'items'):
                return or_dict
            if hasattr(arg, '__getitem__'):
                return True
            if hasattr(arg, '__iter__'):
                return or_seq
        return False
    # (progn (forward-line 1) (snip-insert-mode "py.f.sequence_type" t) (insert "\n"))
    _st_strg = (True,  False, False, False)
    _st_list = (False, True,  False, False)
    _st_dict = (False, False, True,  False)
    _st_seq  = (False, False, False, True)
    _st_none = (False, False, False, False)
    def sequence_type(value):
        if isstring(value):
            return _st_strg
        if hasattr(value, 'items'):
            return _st_dict
        if hasattr(value, '__getitem__'):
            return _st_list
        if hasattr(value, '__iter__'):
            return _st_seq
        return _st_none

# (progn (forward-line 1) (snip-insert-mode "py.f.uchar" t) (insert "\n"))
def uchar(num):
    '''Make UNICODE character.'''
    return eval(sformat(UCHAR_FMT,'\\', num))

# (progn (forward-line 1) (snip-insert "py.b.strings" t t "py") (insert "\n"))
def _ucs(string, charset=None):
    return unicode(string, charset or 'utf-8')
try:
    _ucs("")
except NameError:
    _ucs = lambda s, c=None: s.decode(c or 'utf-8')

uc_type = type(_ucs(b""))

def ucs(value, charset=None):
    if isstring(value) and not isinstance(value, uc_type):
        return _ucs(value, charset)
    return value

def u8s(string, encoding=None):
    if isinstance(string, uc_type):
        return string.encode(encoding or 'utf-8')
    return string

def nts(string):
    # for python3, unicode strings have type str
    if isinstance(string, str):
        return string
    # for python2, encode unicode strings to utf-8 strings
    if isinstance(string, uc_type):
        return string.encode('utf-8')
    if isstring(string):
        try:
            return str(string.decode('utf-8'))
        except UnicodeDecodeError:
            #return str(string.decode('latin1'))
            pass
    return string

# re-arrange sys.path (strangely enough,
# `/usr/lib/python2.7/dist-packages` appears before many
# `/usr/local/lib/python2.7/dist-packages` eggs)

_last_local = None
_new_path = []
_std_path = []
for _dir in sys.path:
    if _dir.startswith('/usr/local/'):
        _last_local = _dir
    else:
        if _dir.endswith('/dist-packages') or _dir.endswith('/site-packages'):
            _std_path.append(_dir)
            continue
    _new_path.append(_dir)

if _std_path and _last_local is not None:
    _first_std_indx = sys.path.index(_std_path[0])
    _last_local_indx = sys.path.index(_last_local)
    if _last_local_indx > _first_std_indx:
        printe(sformat(
            "#    "":PRC:    moved {0} from {1} to {2}",
            _std_path,
            [sys.path.index(_d) for _d in _std_path],
            _last_local_indx + 1))
        _insert_indx = _new_path.index(_last_local) + 1
        _new_path[_insert_indx:_insert_indx] = _std_path
        sys.path = _new_path

here = os.path.abspath(os.path.dirname(__file__))
top_dir = os.path.abspath(os.path.join(here, os.path.pardir))
sys.path.insert(0, top_dir)
sys.path.insert(0, here)

# |:here:|


# -- Project information -----------------------------------------------------

project = u'VC Ignore'
copyright = u'2020, Wolfgang Scherer'
author = u'Wolfgang Scherer'

# The short X.Y version
version = u'0.0'
# The full version, including alpha/beta/rc tags
release = u'0.0'


# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.doctest',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
    'sphinx.ext.coverage',
    'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig',
    'sphinx.ext.viewcode',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [u'_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = None


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'alabaster'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
# html_sidebars = {}


# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'VCIgnoredoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'VCIgnore.tex', u'VC Ignore Documentation',
     u'Wolfgang Scherer', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'vcignore', u'VC Ignore Documentation',
     [author], 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'VCIgnore', u'VC Ignore Documentation',
     author, 'VCIgnore', 'One line description of project.',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Extension configuration -------------------------------------------------

# -- Options for intersphinx extension ---------------------------------------

# Example configuration for intersphinx: refer to the Python standard library.
intersphinx_mapping = {'https://docs.python.org/': None}

# -- Options for todo extension ----------------------------------------------

# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True

# |:here:|

numfig = True
numfig_format = {
    'section': 'section {number}, {name}',
    'figure': 'figure %s',
    'table': 'table %s',
    'code-block': 'listing %s',
}

# --------------------------------------------------
# |||:sec:||| Theme
# --------------------------------------------------

my_preferred_themes = [
    # arrange as needed
    #'revealjs', # for slides
    'bootstrap',
    'guzzle',
    'sphinx_rtd_theme',
    'alabaster',
    'default',
]

for _my_preferred_theme in my_preferred_themes:

    if _my_preferred_theme == 'revealjs':
        try:
            import sphinxjp.themes.revealjs
            html_theme = 'revealjs'
            html_theme_path = [sphinxjp.themes.revealjs.get_path()]
            html_use_index = False
            html_theme_options = {
                "width": 1920,
                "height": 1080,
                "margin": 0,
                # Theme (dejavu/beige/black/blood/league/moon/night/serif/simple/sky/solarized/white)
                "theme": "dejavu",
            }
            break
        except ImportError:
            continue

    if _my_preferred_theme == 'bootstrap':
        try:
            import sphinx_bootstrap_theme
            printe(sformat("#    "":DBG:    {1:<{0}s}: ]{2!s}[", dbg_fwid,
                           "bootstrap", (sphinx_bootstrap_theme.__version__)))
            html_theme = 'bootstrap'
            html_theme_path = sphinx_bootstrap_theme.get_html_theme_path()
            #html_sidebars = {'**': ['localtoc.html', 'relations.html', 'sourcelink.html', 'searchbox.html']}
            html_theme_options = {
                # HTML navbar class (Default: "navbar") to attach to <div> element.
                # For black navbar, do "navbar navbar-inverse"
                "navbar_class": "navbar navbar-inverse",
                'navbar_title': project,
                'navbar_links': [
                    # ('Index', 'genindex'),
                    # ('Modules', 'py-modindex'),
                    # ('Some', 'http://where.over.com', True), # arbitrary link
                ],
                # Global TOC depth for "site" navbar tab. (Default: 1)
                # Switching to -1 shows all levels.
                # 'globaltoc_depth': 2,

                # Choose Bootstrap version.
                # Values: "3" (default) or "2" (in quotes)
                # 'bootstrap_version': "3",

                # Currently, the supported themes are:
                # - Bootstrap 2: https://bootswatch.com/2
                # - Bootstrap 3: https://bootswatch.com/3
                'bootswatch_theme': "cerulean",
            }
            break
        except ImportError:
            continue

    if _my_preferred_theme == 'guzzle':
        try:
            import guzzle_sphinx_theme
            html_theme = 'guzzle_sphinx_theme'
            # # Adds an HTML table visitor to apply Bootstrap table classes
            html_translator_class = "guzzle_sphinx_theme.HTMLTranslator"
            html_theme_path = guzzle_sphinx_theme.html_theme_path()
            # html_theme = "guzzle_sphinx_theme"
            # # Register the theme as an extension to generate a sitemap.xml
            extensions.append("guzzle_sphinx_theme")
            # # Guzzle theme options (see theme.conf for more information)
            html_theme_options = {
                # Set the name of the project to appear in the sidebar
                "project_nav_name": project,
            }
            break
        except ImportError:
            continue

    if _my_preferred_theme == 'sphinx_rtd_theme':
        try:
            import sphinx_rtd_theme
            html_theme = 'sphinx_rtd_theme'
            html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]
            break
        except ImportError:
            continue

    if _my_preferred_theme == 'alabaster':
        try:
            import alabaster
            html_theme = 'alabaster'
            break
        except ImportError:
            continue

    html_theme = 'default'
    break

# --------------------------------------------------
# |||:sec:||| Configuraiton overrides
# --------------------------------------------------

try:
    if version:
        have_version = True
except NameError:
    have_version = False

if not have_version:
    _version_file = '.version'
    for _version_dir in (
        os.path.curdir,
        os.path.pardir,
        os.path.join(os.path.pardir, os.path.pardir),
        os.path.join(os.path.pardir, os.path.pardir, os.path.pardir),
    ):
        _version_path = os.path.join(_version_dir, _version_file)

        if os.path.exists(_version_path):
            _version = []
            with open(_version_path, 'r') as _fh:
                _version = _fh.read().splitlines()
            if _version:
                release = ucs(_version[0])
                version = release
                #version = ucs('.').join(release.split('.')[:2])
                have_version = version
                if have_version:
                    break

if have_version:
    html_title = project + ' v' + release
else:
    html_title = project

source_suffix = ['.rst', '.rst.auto']

latex_documents = [
    (master_doc, 'VCIgnore.tex', project, author, 'manual'),
]

# '' (empty) or 'on' (non-empty)
sd_latex_lstfigures = 'on'
sd_latex_lsttables = 'on'
sd_latex_lstcodeblocks = 'on'
sd_latex_delay_toc = True

if sd_latex_delay_toc:
    sd_latex_delay_toc_comment = ''
else:
    sd_latex_delay_toc_comment = '%%'

latex_elements = {}
latex_elements['papersize'] = 'a4paper'
latex_elements['pointsize'] = '10pt'
latex_elements['extraclassoptions'] = 'openany'

sd_latex_preamble = r'''
%% box drawing characters used for e.g. RAKE symbol
\usepackage{pmboxdraw}

%% --------------------------------------------------
%% |:sec:| unicode characters
%% --------------------------------------------------
\ifdefined\DeclareUnicodeCharacter
%% check mark
\DeclareUnicodeCharacter{221A}{{\tiny\raisebox{1ex}[\ht\strutbox][0pt]{$\sqrt{}$}}}
%% ZERO WIDTH SPACE
\DeclareUnicodeCharacter{200B}{}
%% SYMBOL FOR SPACE
\DeclareUnicodeCharacter{2420}{{\tiny\raisebox{1ex}[.5\ht\strutbox][0pt]{S}{P}}}
%% SHOULDERED OPEN BOX
\DeclareUnicodeCharacter{237D}{\textvisiblespace}
\fi

%% --------------------------------------------------
%% |:sec:| use shortened running title in page header
%% --------------------------------------------------
\let\sdmaketitle=\maketitle
%%\renewcommand{\maketitle}{%
%% \sdmaketitle
%%\title{Short title}%
%%}%

%% --------------------------------------------------
%% |:sec:| add list of figures, list of tables, list of code blocks to TOC
%% --------------------------------------------------
\makeatletter
\def\sdlstfigures{@sd_latex_lstfigures@}%
\def\sdlsttables{@sd_latex_lsttables@}%
\def\sdlstcodeblocks{@sd_latex_lstcodeblocks@}%
\@ifclassloaded{sphinxmanual}{
\let\sdchapter\chapter
\newcommand{\sdseclevel}{chapter}
\newcommand{\sdformanual}[1]{#1\ignorespaces}%
\newcommand{\sdforhowto}[1]{\ignorespaces}%
}{%
\let\sdchapter\section
\newcommand{\sdseclevel}{section}
\newcommand{\sdformanual}[1]{\ignorespaces}%
\newcommand{\sdforhowto}[1]{#1\ignorespaces}%
}%
\newcommand{\sdclearpage}{%
  \sdformanual{\if@openright\cleardoublepage\else\clearpage\fi}}%
\newcommand{\sdpagenumbering}[1]{%
  \sdformanual{\pagenumbering{#1}}}%
\renewcommand{\sphinxtableofcontents}{%
  %
  % before resetting page counter, let's do the right thing.
  \sdclearpage
  \sdpagenumbering{roman}%
  \begingroup
    \parskip \z@skip
    \tableofcontents
  \endgroup
  %
  %% addtional lists
  \ifx\sdlstfigures\empty\relax\else
  \sdclearpage
  \addcontentsline{toc}{\sdseclevel}{List of Figures}%
  \listoffigures
  \fi
  %
  \ifx\sdlsttables\empty\relax\else
  \sdclearpage
  \addcontentsline{toc}{\sdseclevel}{List of Tables}%
  \listoftables
  \fi
  %
  \ifx\sdlstcodeblocks\empty\relax\else
  \sdclearpage
  \addcontentsline{toc}{\sdseclevel}{List of Code Blocks}%
  \listof{literalblock}{List of Code Blocks}%
  \fi
  %
  \sdclearpage
  \sdpagenumbering{arabic}%
}
\makeatother

%% --------------------------------------------------
%% |:sec:| delayed table of contents
%% --------------------------------------------------
\let\sdsphinxtableofcontents=\sphinxtableofcontents
@sd_latex_delay_toc_comment@\let\sphinxtableofcontents\relax
%
\newcommand{\sddlytoctableofcontents}{%
  %% if \sphinxtableofcontents is redefined as \relax, then expand the
  %% original defintion of \sphinxtableofcontents
  \ifx\sphinxtableofcontents\relax
    \pagestyle{plain}%
    \sdsphinxtableofcontents
    \pagestyle{normal}%
  \fi
  %% one-shot execution, disable special delay commands
  \let\sddlytoctableofcontents\relax
  \renewcommand{\sddlytocignore}[1]{##1\ignorespaces}
}
%% helper function for gobbling up pseudo section entries
@sd_latex_delay_toc_comment@\newcommand{\sddlytocignore}[1]{\ignorespaces}

%% --------------------------------------------------
%% |:sec:| appendix |:todo:| subsubsection does not work
%% --------------------------------------------------
\makeatletter

% A nonum chapter : chapter* with pahantomsection to rectify hyperref bug
\@ifclassloaded{sphinxmanual}{%
  \let\aochapter\chapter
  \def\nonumchapter#1{%
    \phantomsection%
    \addcontentsline{toc}{chapter}{#1}%
    \aochapter*{#1}%
  }%
}{%
  \newcommand{\aochapter}[#1]{}%
}%
\let\aosection\section
\def\nonumsection#1{%
  \phantomsection%
  \addcontentsline{toc}{section}{#1}%
  \aosection*{#1}%
}
\let\aosubsection\subsection
\def\nonumsubsection#1{%
  \phantomsection%
  \addcontentsline{toc}{subsection}{#1}%
  \aosubsection*{#1}%
}
\let\aosubsubsection\subsubsection
\def\nonumsubsubsection#1{%
  \phantomsection%
  \addcontentsline{toc}{subsubsection}{#1}%
  \aosubsubsection*{#1}%
}

%% appendix counters
\@ifclassloaded{sphinxmanual}{%
  \newcounter{achapter}
  \setcounter{achapter}{0}
  \newcounter{asection}[achapter]
}{%
  \newcounter{asection}
}%
\setcounter{asection}{0}
\newcounter{asubsection}[asection]
\setcounter{asubsection}{0}
\newcounter{asubsubsection}[asubsection]
\setcounter{asubsubsection}{0}

%% appendix sections
\@ifclassloaded{sphinxmanual}{%
  \newcommand\achapter[1]{%
    \stepcounter{achapter}%
    \nonumchapter{\appendixname~\Alph{achapter}.\quad#1}%
    \edef\@currentlabel{\Alph{achapter}}%
  }%
  \newcommand\asection[1]{%
    \stepcounter{asection}%
    \nonumsection{\Alph{achapter}.\arabic{asection}.\quad#1}%
    \edef\@currentlabel{\Alph{achapter}.\arabic{asection}}%
  }%
  \newcommand\asubsection[1]{%
    \stepcounter{asubsection}%
    \nonumsubsection{\Alph{achapter}.\arabic{asection}.\arabic{asubsection}.\quad#1}%
    \edef\@currentlabel{\Alph{achapter}.\arabic{asection}.\arabic{asubsection}}%
  }%
  \newcommand\asubsubsection[1]{%
    \stepcounter{asubsubsection}%
    \nonumsubsubsection{\Alph{achapter}.\arabic{asection}.\arabic{assubection}.\arabic{asubsubsection}.\quad#1}%
    \edef\@currentlabel{\Alph{achapter}.\arabic{asection}.\arabic{asubsection}.\arabic{asubsubsection}}%
  }%
}{%
  \newcommand\achapter[1]{}%
  \newcommand\asection[1]{%
    \stepcounter{asection}%
    \nonumsection{\appendixname~\Alph{asection}.\quad#1}%
    \edef\@currentlabel{\Alph{asection}}%
  }%
  \newcommand\asubsection[1]{%
    \stepcounter{asubsection}%
    \nonumsubsection{\Alph{asection}.\arabic{asubsection}.\quad#1}%
    \edef\@currentlabel{\Alph{asection}.\arabic{asubsection}}%
  }%
  \newcommand\asubsubsection[1]{%
    \stepcounter{asubsubsection}%
    \nonumsubsubsection{\Alph{asection}.\arabic{asubsection}.\arabic{asubsubsection}.\quad#1}%
    \edef\@currentlabel{\Alph{asection}.\arabic{asubsection}.\arabic{asubsubsection}}%
  }%
}%

%% renewed appendix command
\let\aoappendix\appendix
\newcommand{\sdappendix}{%
  \aoappendix
  \let\chapter\achapter
  \let\section\asection
  \let\subsection\asubsection
  \let\subsubsection\asubsubsection
}%

\newcommand{\sdendappendix}{%
  \let\chapter\aochapter
  \let\section\aosection
  \let\subsection\aosubsection
  \let\subsubsection\aosubsubsection
}%
\makeatother
'''
for _ph, _repl in (
        ('@sd_latex_lstfigures@', sd_latex_lstfigures),
        ('@sd_latex_lsttables@', sd_latex_lsttables),
        ('@sd_latex_lstcodeblocks@', sd_latex_lstcodeblocks),
        ('@sd_latex_delay_toc_comment@', sd_latex_delay_toc_comment),
        ):
    sd_latex_preamble = sd_latex_preamble.replace(_ph, _repl)
latex_elements['preamble'] = sd_latex_preamble

# decrease fontsize for code-blocks
latex_elements['fvset'] = '\\fvset{fontsize=\\footnotesize}'

# --------------------------------------------------
# |||:sec:||| Internal Extensions
# --------------------------------------------------

#extensions = [_e for _e in extensions if _e != 'sphinx.ext.mathjax']
#extensions.append('sphinx.ext.imgmath')

imgmath_image_format = 'svg'

# graphviz internal
extensions.append('sphinx.ext.graphviz')

#graphviz_dot
#graphviz_dot_args =
graphviz_output_format = 'svg'

# --------------------------------------------------
# |||:sec:||| Docutils Comment Role
# --------------------------------------------------

try:
    import du_comment_role
    extensions.append('du_comment_role')
except:
    from docutils.parsers.rst import directives
    from docutils.parsers.rst.languages import en
    from docutils.parsers.rst.roles import register_canonical_role
    def icomment_role(role, rawtext, text, lineno, inliner, options={}, content=[]):
        return [], []
    en.roles['icomment']  = 'icomment'
    register_canonical_role('comment', icomment_role)
    icomment_role.options = {
        'format': directives.unchanged,
        'raw': directives.flag,
        }
    en.roles['span']  = 'span'
    register_canonical_role('span', icomment_role)

# --------------------------------------------------
# |||:sec:||| External Extensions
# --------------------------------------------------

check_extensions = []

if 'sphinxjp.themes.revealjs' in sys.modules:
    check_extensions.append('sphinxjp.themes.revealjs')

#check_extensions.append('ws_docutils.span')
check_extensions.append('sphinxcontrib.ws_figure_container')
figctr_bars = 'single' # 'single', 'both', 'always', 'none'
check_extensions.append('sphinxcontrib.plantuml')

# plantuml = 'java -jar /path/to/plantuml.jar'
# plantuml_output_format = 'png'
plantuml_output_format = 'svg'
# plantuml_latex_output_format = 'png'
plantuml_latex_output_format = 'pdf'
# plantuml_epstopdf = 'epstopdf'

#check_extensions.append('sphinxcontrib.blockdiag')
#check_extensions.append('sphinxcontrib.nwdiag')
#check_extensions.append('sphinxcontrib.makedomain')

# extensions which are not automatically installed
#check_extensions.append('sphinxcontrib.needs')
#check_extensions.append('sphinxcontrib.mercurial')
#check_extensions.append('sphinxcontrib.yuml')

# check availability
for _ext in check_extensions:
    try:
        exec("import " + _ext, {})
        extensions.append(_ext)
    except:
        print('warning: extension {0} failed'.format(_ext))

diag_fonts = [
    '/usr/share/fonts/truetype/freefont/FreeSans.ttf',
    '/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf',
    '/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf',
    # '/usr/share/fonts/truetype/freefont/FreeSansBold.ttf',
    # '/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf',
    # '/usr/share/fonts/truetype/dejavu/DejaVuSerif-Bold.ttf',
]

# Fontpath for blockdiag (truetype font)
blockdiag_fontpath = diag_fonts
blockdiag_transparency = 'sphinxjp.themes.revealjs' not in sys.modules
blockdiag_html_image_format = "png"
blockdiag_latex_image_format = "pdf"

# Fontpath for nwdiag series (truetype font)
nwdiag_fontpath = blockdiag_fontpath
nwdiag_transparency = blockdiag_transparency
Nwdiag_html_image_format = blockdiag_html_image_format
nwdiag_latex_image_format = blockdiag_latex_image_format

# --------------------------------------------------
# |||:sec:||| Setup
# --------------------------------------------------

def setup(app):
    if 'sphinxjp.themes.revealjs' in sys.modules:
        if os.path.exists('_static/ws-revealjs.css'):
            app.add_stylesheet('ws-revealjs.css')
    else:
        if os.path.exists('_static/bootstrap3-sub-menu.css'):
            app.add_stylesheet('bootstrap3-sub-menu.css')
        # if os.path.exists('_static/bootstrap-scrollable-menu.css'):
        #     app.add_stylesheet('bootstrap-scrollable-menu.css')
        if os.path.exists('_static/bootstrap-citation-label.css'):
            app.add_stylesheet('bootstrap-citation-label.css')
        if os.path.exists('_static/wsx-tables.js'):
            app.add_javascript('wsx-tables.js')
        if os.path.exists('_static/wsx-hash-reposition.js'):
            app.add_javascript('wsx-hash-reposition.js')

# :ide: COMPILE: Standard
# . (let ((args "")) (compile (concat "make -k " args)))

# :ide: COMPILE: images
# . (let ((args "images")) (compile (concat "rm -f table_generator.png; make -k " args)))

# :ide: COMPILE: epub
# . (let ((args "epub")) (compile (concat "make -k " args)))

# :ide: COMPILE: latexpdf
# . (let ((args "latexpdf")) (compile (concat "make -k " args)))

# :ide: COMPILE: html
# . (progn (save-buffer) (compile (concat "make -k html")))

# :ide: +-#+
# . Compile ()

#
# Local Variables:
# mode: python
# comment-start: "#"
# comment-start-skip: "#+"
# comment-column: 0
# truncate-lines: t
# End:
