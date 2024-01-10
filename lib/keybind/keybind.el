(keymap-global-set "C-," 'hippie-expand)
(keymap-global-set "C-c d" 'duplicate-dwim)
(keymap-global-set "C--" 'backward-whitespace)
(keymap-global-set "C-=" 'forward-whitespace)
(keymap-global-set "C-z" 'zap-up-to-char)
(keymap-global-set "C-c ," 'replace-string)
(keymap-global-set "C-c ." 'replace-regexp)
(keymap-global-set "C-c 8 f" 'locate)

(define-prefix-command 'hop-keymap)
(define-key hop-keymap "u" 'hop-line)
(keymap-global-set "C-c u" 'hop-keymap)

(define-prefix-command 'wymux-prefix-map)
(keymap-global-set "C-t" 'wymux-prefix-map)
(keymap-global-set "C-c \\" 'eacl-complete-line)
(keymap-global-set "M-z" 'zap-to-char)

(keymap-global-set "M-o" 'wymux/transpose-line-other-buffer)
