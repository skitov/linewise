# linewise

line manipulation package for Emacs.

## Installation

```sh
git clone https://github.com/skitov/linewise
```

## Package description

Package functions are dealing with "selected lines". The definition of selected lines:
* If (and transient-mark-mode mark-active), region extended to include all the lines intersecting with region, including trailing linefeed.
* Otherwise, line containing point including trailing linefeed.

Package functions are:
*   linewise-move-up-or-down        - moves selected lines by number of lines specified by prefix arg
*   linewise-move-up                - moves selected lines one line up
*   linewise-move-down-fast         - moves selected lines 10 lines down
*   linewise-move-up-fast           - moves selected lines 10 lines up
*   linewise-kill                   - kill selected lines
*   linewise-delete                 - delete selected lines without adding to kill ring
*   linewise-copy                   - put selected lines to kill ring without deleting
*   linewise-yank                   - yank at the beginning of current line (yanks previosly killed lines before current)
*   linewise-toggle-comment-out     - comment/uncomment selected lines
*   linewise-repeat                 - fork selected lines
*   linewise-narrow                 - narrow to selected lines
*   linewise-copy-other-window      - copy selected lines to other window and stay there if called without argument or returns to initial window if called with t argument.


## Configuration

Add the following to your emacs config:

```lisp
(add-to-list 'load-path "~/path/to/linewise")
(require 'linewise)
```
If you want default keybinding add:

```lisp
;;   (linewise-set-hotkeys)
```
or:
```lisp
;;   (linewise-set-hotkeys t)
```

In first case prefix will be "C-c l", in second: "C-l".
default keybinding is:
*   "\<M-S-down\>" 'linewise-move-up-or-down      - moves selected lines by number of lines specified by prefix arg
*   "\<M-S-up\>" 'linewise-move-up                - moves selected lines one line up
*   "\<escape\> \<down\>" 'linewise-move-down-fast  - moves selected lines 10 lines down
*   "\<escape\> \<up\>" 'linewise-move-up-fast      - moves selected lines 10 lines up
*   "prefix k" 'linewise-kill                   - kill selected lines
*   "prefix d" 'linewise-delete                 - delete selected lines without adding to kill ring
*   "prefix c" 'linewise-copy                   - put selected lines to kill ring without deleting
*   "prefix y" 'linewise-yank                   - yank at the beginning of current line (yanks previosly killed lines before current)
*   "prefix h" 'linewise-toggle-comment-out     - comment/uncomment selected lines
*   "prefix r" 'linewise-repeat                 - fork selected lines
*   "prefix n" 'linewise-narrow                 - narrow to selected lines
*   "prefix o" 'linewise-copy-other-window      - copy selected lines to other window and stay there
*   "prefix C-o" '(lambda() (interactive) (linewise-copy-other-window t)) - copy selected lines to other window and return back to initial window

## Copyright

Copyright (c) 2017-2024 Sergey Kitov.
