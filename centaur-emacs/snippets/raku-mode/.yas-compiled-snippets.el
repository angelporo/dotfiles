;;; "Compiled" snippets and support files for `raku-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'raku-mode
                     '(("whileg" "$0 while ${1:condition};" "whileg" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/whileguard"
                        nil nil)
                       ("while" "while ${1:condition} {\n  $0\n}\n" "while" nil
                        nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/while"
                        nil nil)
                       ("untilg" "$0 until ${1:condition};" "untilg" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/untilguard"
                        nil nil)
                       ("until" "until ${1:condition} {\n\n}" "until" nil nil
                        nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/until"
                        nil nil)
                       ("ung" "$0 unless ${1:condition};" "unless guard" nil nil
                        nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/unless-guard"
                        nil nil)
                       ("un" "unless ${1:condition} {\n  $0\n}" "unless" nil nil
                        nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/unless"
                        nil nil)
                       ("tern" "${1:condition} ?? ${2:true} !! ${3:false};"
                        "Ternary Operator" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/ternary-operator"
                        nil nil)
                       ("subset"
                        "subset ${1:name} of ${2:(Type)} where { $_ $0}"
                        "subset" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/subset"
                        nil nil)
                       ("sub" "sub ${1:subrutine}${2:(${3:args})} {\n  $0\n}\n"
                        "Subrutine" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/subrutine"
                        nil nil)
                       ("raku-mode" "/Users/toni/.emacs.d/snippets/raku-mode"
                        "raku-mode" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/raku-mode"
                        nil nil)
                       ("pod" "=begin ${1:comment}\n \n$0\n \n=end $1" "Pod" nil
                        nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/pod" nil
                        nil)
                       ("co" "#`($0)" "multi-line and in-the-middle comments"
                        nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/multi-line-comment"
                        nil nil)
                       ("multi"
                        "multi ${1:subrutine}${2:(${3:args})} {\n  $0\n}\n"
                        "multi" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/multi"
                        nil nil)
                       ("main" "sub MAIN${2:(${3:args})} {\n  $0\n}\n" "main"
                        nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/main"
                        nil nil)
                       ("ifg" "$0 if ${1:condition};" "if guard" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/ifguard"
                        nil nil)
                       ("ife" "if ${1:condition} {\n  $2\n} else {\n  $3\n}"
                        "if else" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/if-else"
                        nil nil)
                       ("if" "if ${1:condition} {\n  $0\n}" "if" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/if" nil
                        nil)
                       ("given"
                        "given ${1:variable} {\n  when ${2:condition} { ${3:block} }\n  when ${4:condition} { ${5:block} }\n  default { ${6:default-block} }\n}"
                        "given" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/given"
                        nil nil)
                       ("forw" "for ${1:list} -> ${2:i} is rw {\n  $0\n}\n"
                        "for variable is rw" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/forw"
                        nil nil)
                       ("forvc" "for ${1:list} -> ${2:i} is copy {\n  $0\n}\n\n"
                        "for variable is copy" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/forvc"
                        nil nil)
                       ("forv" "for ${1:list} -> ${2:i} {\n  $0\n}\n"
                        "for variable" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/forv"
                        nil nil)
                       ("forg" "$0 for ${1:list}\n" "for-guard" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/for-guard"
                        nil nil)
                       ("for" "for ${1:lista} {\n  $0\n}\n" "for" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/for" nil
                        nil)
                       ("ifi"
                        "if ${1:condition} {\n  $2\n} elsif ${3:condition} {\n  $4\n} else {\n  $5\n}\n"
                        "elsif" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/raku-mode/elsif"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:34 2025
