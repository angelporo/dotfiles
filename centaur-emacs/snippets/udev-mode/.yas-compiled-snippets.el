;;; "Compiled" snippets and support files for `udev-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'udev-mode
                     '(("run" "RUN+=\"$0\"" "run" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/run" nil
                        nil)
                       ("$" "$env{$1} $0" "env$" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/env$"
                        nil nil)
                       ("add" "ACTION==\"add\", $0" "add" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/add" nil
                        nil)
                       ("ker" "KERNEL!=\"$0\"" "KERNEL" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/KERNEL"
                        nil nil)
                       ("goto" "GOTO=\"$1\"\n$0\n\nLABEL=\"$1\"" "GOTO" nil nil
                        nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/GOTO"
                        nil nil)
                       ("env" "ENV{$1}$0" "ENV" nil nil nil
                        "/Users/liyuan/.config/emacs/snippets/udev-mode/ENV" nil
                        nil)))


;;; Do not edit! File generated at Tue Sep  9 15:36:35 2025
