;;; "Compiled" snippets and support files for `antlr-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'antlr-mode
                     '(("target"
                        "<target name=\"${1:compile}\" ${2:other}>\n        $0\n</target>"
                        "target" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/antlr-mode/target"
                        nil nil)
                       ("prop"
                        "<property name=\"${1:name}\" value=\"${2:value}\" />\n$0"
                        "property" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/antlr-mode/property"
                        nil nil)
                       ("proj"
                        "<project name=\"${1:test}\" default=\"${2:compile}\" basedir=\"${3:.}\">\n\n$0\n</project>"
                        "project" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/antlr-mode/project"
                        nil nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:33 2025
