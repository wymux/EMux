(electric-pair-mode 1)
(global-font-lock-mode -1)
(global-display-line-numbers-mode -1)
(global-eldoc-mode -1)
(global-hl-line-mode 1)
(fringe-mode 4)
(menu-bar-mode -1)
(tool-bar-mode -1)
(recentf-mode 1)
(save-place-mode 1)
(scroll-bar-mode -1)
(window-divider-mode 1)

(customize-set-variable
 'major-mode-remap-alist
 '(()))

(add-to-list
 'auto-mode-alist
 '("\\.go?\\'" . wymux-go-mode))

(add-to-list
 'auto-mode-alist
 '("\\.rs?\\'" . rust-ts-mode))
			

