;;; "Compiled" snippets and support files for `makefile-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-mode
                     '(("cl" "clean:\n	${1:rm -r ${2:\\$(${3:OUTDIR})}}\n$0\n"
                        "clean" nil nil ((yas-indent-line 'fixed))
                        "/Users/liyuan/.config/emacs/snippets/makefile-mode/clean"
                        nil nil)
                       ("all" "all:\n        $0" "all" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/makefile-mode/all"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:34 2025
