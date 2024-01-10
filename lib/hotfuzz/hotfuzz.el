(add-to-list 'load-path "~/Internet/Git/Emacs/hotfuzz/")
(wymux/add-load-path "hotfuzz/")

(require 'hotfuzz)

(customize-set-variable 'completion-styles '(hotfuzz))
(customize-set-variable 'hotfuzz-max-highlighted-completions 0)
(customize-set-variable 'completion-ignore-case t)
(customize-set-variable 'read-file-name-completion-ignore-case t)
(customize-set-variable 'read-buffer-completion-ignore-case t)

(hotfuzz-vertico-mode)
