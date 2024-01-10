(add-to-list 'load-path (concat wymux-emacs-package-dir "xelb/"))
(add-to-list 'load-path (concat wymux-emacs-package-dir "exwm/"))

(require 'xelb)
(require 'exwm)
(require 'exwm-randr)

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(customize-set-variable 'exwm-workspace-number 2)

(setq exwm-input-global-keys
      `(([?\s-a] . execute-extended-command)
	([?\s-d] . kill-buffer-and-window)
	([?\s-e] . eshell)
	([?\s-f] . find-file)
	([?\s-k] . delete-window)
	([?\s-`] . jump-to-register)
	([?\s-o] . other-window)
	([?\s-r] . exwm-reset)
        ([?\s-l] . wymux/prismlauncher)
        ([?\s-w] . wymux/firefox)
	([?\s-r] . wymux/recentf-find)
	([?\s-s] . save-buffer)
	([?\s-t] . switch-to-buffer)
	([?\s-q] . save-buffers-kill-emacs)
	([?\s-p] . ffap)
	([?\s-m] . mh-nmail)
	([?\s-x] . kill-this-buffer)
	([?\s--] . previous-buffer)
	([?\s-=] . next-buffer)
	([f6] . other-frame)
	([?\s-3] . split-window-horizontally)
	([?\s-4] . split-window-vertically)
	([f5] . other-window)
	([f6] . other-frame)
	([f8] . wymux/open-document)
	([f9] . emms)
	([f10] . emms-play-directory-tree)
	([f11] . emms-play-find)
	([f12] . wymux/invert)
	([s-f2] . wymux/decrease-volume)
	([s-f3] . wymux/increase-volume)
	([s-f4] . wymux/toggle-volume)
	([XF86MonBrightnessDown] . wymux/decrease-brightness)
	([XF86MonBrightnessUp] . wymux/increase-brightness)))

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\C-j] . [return])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
	([?\C-y] . [paste])))

(exwm-randr-enable)
(exwm-enable)

(wymux/monitor)

(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

;;; Rename buffer to window title.
(defun exwm-rename-buffer-to-title ()
  ""
  (exwm-workspace-rename-buffer exwm-title))

(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)

(customize-set-variable 'exwm-manage-configurations 
			'(((member exwm-class-name '("*Minecraft\\*" "llpp"))
			   char-mode t)))

(customize-set-variable 'exwm-workspace-minibuffer-position 'nil)
(customize-set-variable 'exwm-workspace-display-echo-area-timeout '1)
