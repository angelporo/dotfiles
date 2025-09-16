;;; "Compiled" snippets and support files for `git-commit-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'git-commit-mode
                     '(("type"
                        "${1:$$(yas-choose-value '(\"fix\" \"hotfix\" \"add\" \"update\" \"change\" \"clean\" \"disable\" \"remove\" \"upgrade\" \"revert\"))} $0"
                        "type" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/git-commit-mode/type"
                        nil nil)
                       ("ref" "references #${1:100}" "references" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/git-commit-mode/references"
                        nil nil)
                       ("fix" "fixes #${1:100}" "fixes" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/git-commit-mode/fixes"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:34 2025
