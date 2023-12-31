(set-face-attribute 'default nil :family "Berkeley Mono" :height 120)
(set-face-attribute 'variable-pitch nil :family "Berkeley Mono Variable" :height 120)

(add-to-list 'custom-theme-load-path "~/.config/emacs/lib/lambda-themes/")
(add-to-list 'load-path "~/.config/emacs/lib/lambda-themes/")

(add-to-list 'custom-theme-load-path "/home/wymux/Internet/Git/Emacs/modus-themes")
(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/modus-themes")

(add-to-list 'custom-theme-load-path "/home/wymux/Internet/Git/Emacs/almost-mono-themes/")
(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/almost-mono-themes/")

(add-to-list 'custom-theme-load-path "/home/wymux/Internet/Git/Emacs/tao-theme-emacs/")
(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/tao-theme-emacs/")

(setq lambda-themes-set-italic-comments nil)
(setq lambda-themes-set-italic-keywords nil)
(setq lambda-themes-set-variable-pitch nil)
(setq tao-theme-sepia-saturation nil)
