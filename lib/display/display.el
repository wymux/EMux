(defvar wymux-inverted nil)

(unless (< (string-to-number (format-time-string "%k")) 20)
  (setq wymux-inverted t))

(defcustom wymux-light-theme 'almost-mono-white "Light theme")
(defcustom wymux-dark-theme 'almost-mono-black "Dark theme")

(defun wymux/invert ()
  ""
  (interactive)
  (if wymux-inverted
      (progn
	(load-theme wymux-light-theme t)
	(disable-theme wymux-dark-theme)
	(setq wymux-inverted nil)
	(set-frame-font "Iosevka 13" nil t))
    (progn
      (load-theme wymux-dark-theme t)
      (disable-theme wymux-light-theme)
      (setq wymux-inverted t)
      (set-frame-font "Iosevka 13" nil t))))

(wymux/invert)

(customize-set-variable 'query-all-font-backends t)

(customize-set-variable
 'display-buffer-alist
 '(("\\*Help\\*\\|*Locate\\*"
    (display-buffer-same-window))
   ((derived-mode . compilation-mode)
    (display-buffer-at-bottom))
   ((derived-mode . Man-mode)
    (display-buffer-same-window))))

(customize-set-variable 'Man-notify-method 'pushy)
(customize-set-variable 'display-line-numbers-type 't) 

(customize-set-variable 'window-divider-default-right-width 2)
(customize-set-variable 'window-divider-default-bottom-width 2)
(customize-set-variable 'window-divider-default-places t)
(setq-default mode-line-format " %b")
(customize-set-variable 'minibuffer-frame-alist '((minibuffer . only)))

(customize-set-variable 'cursor-type 'bar)

(require 'flymake)
(set-face-attribute 'flymake-error nil :underline 'nil)

(setq-default mode-line-format " %b")
