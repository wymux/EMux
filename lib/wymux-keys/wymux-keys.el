(defvar wymux-command-map (make-sparse-keymap))

(define-minor-mode wymux-keys
  ""
  :global t
  :keymap wymux-command-map
  (if wymux-keys
      (progn
	(set-cursor-color "#0000FF")
	(add-hook 'isearch-mode-end-hook 'wymux-keys-enable)
	(add-hook 'minibuffer-setup-hook 'wymux-keys-disable)
	(add-hook 'minibuffer-exit-hook 'wymux-keys-enable))
    (progn
      (set-cursor-color "#FF0000")
      (remove-hook 'isearch-mode-end-hook 'wymux-keys-enable)
      (remove-hook 'minibuffer-setup-hook 'wymux-keys-disable)
      (remove-hook 'minibuffer-exit-hook 'wymux-keys-enable))))

