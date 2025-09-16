;;; "Compiled" snippets and support files for `fish-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'fish-mode
                     '(("while" "while ${1:cond}\n    $0\nend\n" "while loop"
                        nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/while"
                        nil nil)
                       ("sw"
                        "switch ${1:condition}\n    case ${2:*}\n         ${0}\nend\n"
                        "switch" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/sw" nil
                        nil)
                       ("ife"
                        "if ${1:cond}\n    ${2:stuff}\nelse\n    ${3:other}\nend\n$0\n"
                        "if ... ... else ... end" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/ife" nil
                        nil)
                       ("if" "if ${1:[ -f file ]}\n    ${2:do}\nend\n$0"
                        "if ... end" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/if" nil
                        nil)
                       ("function" "function ${1:name}\n    $0\nend\n"
                        "function" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/function"
                        nil nil)
                       ("for" "for ${1:var} in ${2:stuff}\n    $0\nend\n"
                        "for loop" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/for" nil
                        nil)
                       ("bp" "breakpoint\n$0" "breakpoint" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/bp" nil
                        nil)
                       ("block" "begin\n    $0\nend\n" "begin ... end" nil nil
                        nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/block"
                        nil nil)
                       ("!" "#!/usr/bin/env fish\n$0\n" "bang" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/fish-mode/bang"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:34 2025
