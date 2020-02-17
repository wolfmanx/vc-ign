vc-ign - VC Ignore Emacs Package
================================

Author  
[Wolfgang Scherer](wolfgang.scherer@gmx.de)

**Quickstart**

-   Load `vc-ign.el` to augment Emacs package **vc** with VC ignore facilities.
-   The keyboard shortcuts are bound to prefix `z` in *vc-dir-mode* and `C-x v z` in other modes. The prefix can be customized with *vc-ign-prefix*.
-   Press `z z` in *vc-dir-mode* or `C-x v z z` in *dired-mode* to ignore marked files. In other modes, a file is read from the minibuffer. With a prefix argument, the files are removed from the ignore file.
-   Press `z p` in *vc-dir-mode* or `C-x v z p` in *dired-mode* for a prompt with the current file as properly quoted pattern. In other modes, a pattern is read from the minibuffer. With a prefix argument, the pattern is removed from the ignore file
-   Press `z w` in *vc-dir-mode* or `C-x v z w` in other modes to push the marked file names relatve to the repository root onto the *kill-ring*. With a prefix argument, escape and anchor the file names. The file names are concatenated with a newline.

**Abstract**

Introduction
------------

**References**

**Copyright**

Copyright (C) 2020, [Wolfgang Scherer](wolfgang.scherer@gmx.de), &lt;Wolfgang.Scherer at gmx.de&gt;. See the document source for conditions of use under the GNU Free Documentation License.
