(defvar wymux-emacs-package-dir "~/Internet/Git/Emacs/")
(defvar wymux-bin-dir (concat user-emacs-directory "bin/"))
(defvar wymux-lib-dir (concat user-emacs-directory "lib/"))

(defun wymux/load-bin (file)
  "Load lib/'file'/'file'.el"
  (interactive)
  (load-file (concat wymux-bin-dir file "/" file ".el")))

(defun wymux/load-lib (file)
  "Load lib/'file'/'file'.el"
  (interactive)
  (load-file (concat wymux-lib-dir file "/" file ".el")))

(defun wymux/add-load-path (dir)
  ""
  (add-to-list 'load-path (concat wymux-emacs-package-dir dir)))

(setq-default mode-line-format nil)

(wymux/load-lib "backup")
(wymux/load-lib "mode")
(wymux/load-bin "exwm")
(wymux/load-lib "exwm")
(wymux/load-lib "recentf")
(wymux/load-lib "vertico")
(wymux/load-lib "keybind")

(defun backward-whitespace ()
  ""
  (interactive)
  (forward-whitespace -1))

(defun wymux/recentf-find ()
  ""
  (interactive)
  (let ((c (completing-read "File: " recentf-list)))
    (find-file c)))

(customize-set-variable 'use-short-answers t)

(customize-set-variable 'window-divider-default-right-width 2)
(customize-set-variable 'window-divider-default-bottom-width 2)
(customize-set-variable 'window-divider-default-places t)

(wymux/load-lib "abbrev")

(load-file "/usr/share/mailutils/mh/mailutils-mh.el")

(customize-set-variable 'mh-identity-list '(("Wymux"
					     (("cc:" . "archive@shoash.com")
					      ("From:" . "wymux@shoash.com")))))

(customize-set-variable 'mh-identity-default "Wymux")

(wymux/load-lib "emms")

(defvar wymux-inverted t)

(unless (< (string-to-number (format-time-string "%k")) 20)
  (setq wymux-inverted nil))

(defun wymux/invert ()
  ""
  (interactive)
  (if (not wymux-inverted)
      (progn
	(custom-set-faces `(default
			    ((t (:background "#000000" :foreground "#FFFFFF")))))
	(custom-set-faces `(mode-line
			    ((t (:background "#000000" :foreground "#FFFFFF")))))
	(setq wymux-inverted t)
	(set-frame-font "Berkeley Mono 12" t))
    (progn
      (custom-set-faces `(default
			  ((t (:background "#FFFFFF" :foreground "#000000")))))
      (custom-set-faces `(mode-line
			  ((t (:background "#FFFFFF" :foreground "#000000")))))
      (setq wymux-inverted nil)
      (set-frame-font "Berkeley Mono 12" t))))

(wymux/invert)
 
(wymux/load-lib "register")

(defun wymux/create-unavailable-dir ()
  ""
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(add-to-list 'find-file-not-found-functions 'wymux/create-unavailable-dir)

(defun wymux/elevate-permission ()
  ""
  (let ((file (buffer-file-name)))
    (if (not (file-writable-p file))
	(find-alternate-file (concat "/doas::" file)))))

(add-to-list 'find-file-hook 'wymux/elevate-permission)

(load-file "~/Internet/Git/Emacs/ggtags/ggtags.elc")
(load-file "~/Internet/Git/Emacs/emacs-eat/eat.elc")

(add-hook 'eshell-load-hook 'eat-eshell-mode)

(wymux/load-lib "gnus")

(customize-set-variable 'c-default-style "linux")
 
(defun wymux/ggtags ()
  ""
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)))

(add-hook 'c-mode-common-hook 'wymux/ggtags)

(customize-set-variable 'require-final-newline t)

(customize-set-variable
 'display-buffer-alist
 '(("\\*compilation\\*\\|\\*Help\\*\\|*Locate\\*"
    (display-buffer-same-window))
   ((derived-mode . Man-mode)
    (display-buffer-same-window))))

(customize-set-variable 'Man-notify-method 'pushy)
(customize-set-variable 'display-line-numbers-type 't) 

(add-to-list 'load-path "~/Internet/Git/Emacs/hotfuzz/")
(require 'hotfuzz)

(customize-set-variable 'completion-styles '(hotfuzz))

(hotfuzz-vertico-mode 1)

(wymux/load-lib "control-mode")

(control-mode-default-setup)

(defun wymux/go-fmt ()
  ""
  (interactive)
  (shell-command (concat "gofmt -w " (buffer-file-name)))
  (revert-buffer nil t))

(customize-set-variable 'disabled-command-function nil)
(customize-set-variable 'sentence-end-double-space 'nil)

(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)

(customize-set-variable 'mm-text-html-renderer 'links)
(customize-set-variable 'mm-html-blocked-images nil)
(customize-set-variable 'mm-html-inhibit-images nil)
