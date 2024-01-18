(defvar wymux-light-theme t)

(defun wymux/dark-theme ()
  ""
  (interactive)
  (set-foreground-color "#FFFFFF")
  (set-background-color "#000000")
  (setq wymux-light-theme nil))

(defun wymux/bright-theme ()
  ""
  (interactive)
  (set-foreground-color "#000000")
  (set-background-color "#FFFFFF")
  (setq wymux-light-theme t))

(defun wymux/select-theme ()
  ""
  (let ((time-now (string-to-number (format-time-string "%H"))))
    (if (or (>= time-now 4)
	     (<= 22 time-now))
	(wymux/dark-theme)
      (wymux/bright-theme))))

(wymux/select-theme)

(global-font-lock-mode -1)
(electric-pair-mode 1)
(customize-set-variable 'inhibit-splash-screen t)
(add-to-list 'load-path "~/Internet/Git/Emacs/xelb-2")
(add-to-list 'load-path "~/Internet/Git/Emacs/exwm-2")

(require 'exwm)
(require 'exwm-config)
(exwm-enable)
(exwm-config-example)

(defun wymux/brighten ()
  "Increase monitor brightness."
  (interactive)
  (start-process "Brighten" nil "blmgr" "+500"))

(defun wymux/darken ()
  "Decrease monitor brightness."
  (interactive)
  (start-process "Darken" nil "blmgr" "-500"))

(defun wymux/firefox-light ()
  "Load light firefox."
  (interactive)
  (start-process "Firefox" "Firefox-light" "firefox" "-p" "S1"))

(defun wymux/firefox-dark ()
  "Load dark firefox."
  (interactive)
  (start-process "Firefox" "Firefox-dark" "firefox" "-p" "S2"))

(defun wymux/firefox ()
  "Load firefox with theming consistent with Emacs."
  (interactive)
  (let ((buffer "firefox-default"))
    (if (not (get-buffer buffer))
	(if wymux-light-theme
	    (wymux/firefox-light)
	  (wymux/firefox-dark))
      (switch-to-buffer buffer))))

(defun wymux/mpv ()
  "Load video."
  (interactive)
  (let ((dir "~/Media/Video")
	(reg "webm"))
    (start-process "mpv" "mpv" "mpv" (expand-file-name
				      (completing-read "Media: "
						       (directory-files-recursively dir reg))))))

(defun wymux/chromium-light ()
  "Load light chromium"
  (interactive)
  (start-process "Chromium" "Chromium-light" "chromium-browser"))

(setq exwm-input-global-keys
      `(
	([?\s-r] . exwm-reset)
	([kp-end] . kill-emacs)
	([f1] . wymux/darken)
	([f2] . wymux/brighten)
	([s-\[] . wymux/chromium-light)
	([kp-begin] . other-window)
	([kp-up] . split-window-vertically)
	([kp-home] . split-window-horizontally)
	([kp-left] . delete-window)
	([kp-down] . kill-this-buffer)
	([?\s-0] . other-window)
	([kp-delete] . wymux/mpv)
	([kp-prior] . mh-rmail)
	([kp-right] . eshell)
	([f12] . wymux/firefox)
	([kp-multiply] . previous-buffer)
	([kp-divide] . next-buffer)))

(defun wymux/customize-set-variable ()
  ""
  (interactive)
  (let ((basestr "(customize-set-variable \'")
	(var (symbol-name (read-variable "Customize: "))))
    (insert (concat basestr var " nil" ")" "\n"))))

(setq-default abbrev-mode t)

(require 'eshell)
(defun wymux/eshell-ug ()
  ""
  (interactive)
  (insert "cd ~/Internet/Git/")
  (eshell-send-input))

(defun wymux/eshell-umx ()
  ""
  (interactive)
  (insert "cd ~/Internet/Git/Exherbo/")
  (eshell-send-input))

(defun wymux/eshell-um ()
  ""
  (interactive)
  (insert "cd ~/Media/Musica")
  (eshell-send-input))

(defun wymux/eshell-ugp ()
  ""
  (interactive)
  (insert "cd ~/Internet/Git/Project/")
  (eshell-send-input))

(progn
  (when (boundp 'eshell-mode-abbrev-table)
    (clear-abbrev-table eshell-mode-abbrev-table))
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("ug" "" wymux/eshell-ug nil)
      ("umx" "" wymux/eshell-umx nil)
      ("um" "" wymux/eshell-um nil)
      ("ugp" "" wymux/eshell-ugp)
      ("crx" "" "doas cave resolve -x"))))

(progn
  (when (boundp 'emacs-lisp-mode-abbrev-table)
    (clear-abbrev-table emacs-lisp-mode-abbrev-table))
  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(("csv" "" wymux/customize-set-variable nil))))

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "C-t" 'hippie-expand)
(keymap-global-set "C-<backspace>" 'kill-whole-line)

(load-file "/usr/share/mailutils/mh/mailutils-mh.el")

(customize-set-variable 'mh-identity-list '(("Wymux"
					     (("Bcc:" . "archive@shoash.com")
					      ("Fcc:" . "Sent")
					      ("From:" . "wymux@shoash.com")))))

(customize-set-variable 'mh-identity-default "Wymux")
(customize-set-variable 'mm-text-html-renderer 'shr)
(customize-set-variable 'mm-html-blocked-images nil)
(customize-set-variable 'mm-html-inhibit-images nil)
(customize-set-variable 'mh-do-not-confirm-flag t)

(defun wymux/mh-inc-folder-hook()
  "Rescan folder after incorporating mail."
  (if (buffer-modified-p)
      (mh-execute-commands))
  (mh-rescan-folder)
  (mh-show))

(add-hook 'mh-inc-folder-hook 'wymux/mh-inc-folder-hook)

(add-to-list 'load-path "~/Internet/Git/Emacs/hotfuzz/")
(require 'hotfuzz)
(customize-set-variable 'completion-styles '(hotfuzz))
(add-hook 'icomplete-minibuffer-setup-hook
	  (lambda () (setq-local completion-styles '(hotfuzz))))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun wymux/save ()
  ""
  (if (eq major-mode 'emacs-lisp-mode)
      (elisp-byte-compile-file)))
(add-hook 'after-save-hook 'wymux/save)

(require 'emms)
(require 'emms-history)
(require 'emms-playlist-mode)
(require 'emms-player-mpd)
(require 'emms-setup)

(customize-set-variable 'emms-player-mpd-server-name "localhost")
(customize-set-variable 'emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
(customize-set-variable 'emms-player-mpd-music-directory "~/Media/Musica/")
(customize-set-variable 'emms-source-file-default-directory "~/Media/Musica/")
(emms-player-mpd-connect)

(emms-player-set emms-player-mpd
		 'regex
		 "\\(flac\\|mp3\\|ape\\)$")

(load-file "~/Internet/Git/Emacs/emacs-eat/eat.elc")

(add-hook 'eshell-load-hook 'eat-eshell-mode)

(defun backward-whitespace ()
  "Move point to end of the previous sequence of whitespace chars."
  (interactive)
  (forward-whitespace -1))

(defun wymux/emms-play-find ()
  ""
  (interactive)
  (let ((track (read-from-minibuffer "Track: ")))
    (emms-play-find emms-source-file-default-directory track)))

(defun wymux/format-buffer ()
  ""
  (interactive)
  (indent-region (goto-char (point-min)) (goto-char (point-max))))

(load-file "~/Internet/Git/Emacs/modaled/modaled.el")

(modaled-define-state "normal"
  :lighter "[NOR]"
  :cursor-type 'box)

(modaled-define-keys
  :states '("normal")
  :bind
  `(("k" . backward-char)
    (";" . forward-char)
    ("o" . previous-line)
    ("l" . next-line)
    ("[" . backward-paragraph)
    ("]" . forward-paragraph)
    ("g" . set-mark-command)
    ("a" . execute-extended-command)
    ("," . backward-whitespace)
    ("." . forward-whitespace)
    ("t" . repeat)
    ("\\m" . back-to-indentation)
    ("q" . move-beginning-of-line)
    ("w" . move-end-of-line)
    ("s" . mark-defun)
    ("b" . duplicate-dwim)
    ("xo" . wymux/format-buffer)
    ("xf" . find-file)
    ("xr" . recentf-open)
    ("xu" . save-buffer)
    ("\\[" . beginning-of-buffer)
    ("\\]" . end-of-buffer)
    ("\\f" . wymux/emms-play-find)
    ("\\e" . emms)
    ("\\d" . emms-play-directory-tree)
    ("\\n" . magit)
    ("\\<backspace>" . compile)
    ("\\t" . wymux/search-www)
    ("\\r" . wymux/open-document)
    ("u" . backward-word)
    ("i" . forward-word)
    ("y" . yank)
    ("j" . undo)
    ("e" . backward-kill-word)
    ("r" . kill-word)
    ("c" . kill-sexp)
    ("v" . kill-whole-line)
    ("`" . delete-char)
    ("z" . recenter-top-bottom)
    ("h f" . describe-function)
    ("h v" . describe-variable)
    ("h a f" . apropos-function)
    ("p" . wymux/modaled-insert-state)))

(modaled-define-state "insert"
  :sparse t
  :no-suppress t
  :cursor-type 'bar
  :lighter "[INS]")

(modaled-define-keys
  :states '("insert" "normal")
  :bind
  '(
    ([escape] . modaled-set-default-state)
    ([ESCAPE] . modaled-set-default-state)
    ))

(modaled-define-substate "emacs-lisp")
(modaled-define-keys
  :substates '("emacs-lisp")
  :bind
  '((" x" . eval-defun)))

(modaled-enable-substate-on-state-change
  "emacs-lisp"
  :states '("normal")
  :major '(emacs-lisp-mode))

(modaled-define-substate "dired")
(modaled-define-keys
  :substates '("dired")
  :bind
  '(("o" . dired-previous-line)
    ("l" . dired-next-line)
    ("m" . dired-create-directory)
    ("t" . dired-up-directory)
    ("r" . dired-do-rename)))

(modaled-enable-substate-on-state-change
  "dired"
  :states '("insert")
  :major '(dired-mode))

(require 'eglot)

(modaled-define-substate "eglot")
(modaled-define-keys
  :substates '("eglot")
  :bind
  '((" g" . eglot-code-actions)
    (" u" . flymake-goto-next-error)))

(modaled-enable-substate-on-state-change
  "eglot"
  :states '("normal")
  :minor '(eglot--managed-mode))

(modaled-define-default-state
  '("insert" dired-mode wdired-mode eshell-mode eat-eshell-mode
    debugger-mode mh-folder-mode emms-playlist-mode calendar-mode
    magit-status-mode git-commit-mode backtrace-mode info-mode help-mode)
  '("normal"))

(defun wymux/modaled-insert-state ()
  ""
  (interactive)
  (modaled-set-state "insert"))

(add-to-list 'auto-mode-alist
	     '("\\.tsx?\\'" . typescript-ts-mode))

(add-to-list 'auto-mode-alist
	     '("\\.ts?\\'" . typescript-ts-mode))

(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

(recentf-mode 1)

(customize-set-variable 'treesit-font-lock-level 0)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-hook 'typescript-ts-mode-hook 'prettier-mode)
(add-hook 'web-mode-hook 'prettier-mode)

(add-hook 'typescript-ts-mode-hook 'emmet2-mode)

(load-file "~/Internet/Git/Emacs/eacl/eacl.el")
(add-to-list 'load-path "~/Internet/Git/Emacs/compat/")
(load-file "~/Internet/Git/Emacs/vertico/vertico.el")
(add-to-list 'load-path "~/Internet/Git/Emacs/vertico/extensions/")
(load-file "~/Internet/Git/Emacs/emacs-websocket/websocket.el")
(load-file "~/Internet/Git/Emacs/deno-bridge/deno-bridge.el")
(load-file "~/Internet/Git/Emacs/emmet2-mode/emmet2-mode.el")

(vertico-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(prettier)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/Internet/Git/Emacs/iter2/")
(add-to-list 'load-path "~/Internet/Git/Emacs/s.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/f.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/editorconfig-emacs/")
(add-to-list 'load-path "~/Internet/Git/Emacs/nvm.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/with-editor/lisp/")
(add-to-list 'load-path "~/Internet/Git/Emacs/dash.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/transient/lisp/")
(add-to-list 'load-path "~/Internet/Git/Emacs/magit/lisp/")
(add-to-list 'load-path "~/Internet/Git/Utility/emacs-pcre")

(require 'transient)
(require 'magit)
(customize-set-variable 'use-short-answers t)
(setq package-install-upgrade-built-in t)
(save-place-mode 1)

(customize-set-variable 'eglot-confirm-server-initiated-edits nil)

(defvar wymux-search-websites '((libgen . "https://libgen.li/index.php?req=%s&res=100")))

(defun wymux/search-www (str)
  "Search web."
  (interactive "sSearch: ")
  (let ((site (format (cdr (assoc 'libgen wymux-search-websites))
		      (replace-regexp-in-string "\s" "+" str)))
	(website (completing-read "Search Website: " wymux-search-websites))
	(profile "S2")
	(buffer "Firefox-dark")
	(new-tab "--new-tab")
	(ch1 (char-to-string (read-char)))
	(ch2 (char-to-string (read-char))))
    (when (not (get-buffer "firefox-default"))
      (setq new-tab ""))
    (when wymux-light-theme
      (setq profile "S1"
	    buffer "Firefox-light"))
    (message "new-tab: %s, profile: %s" new-tab profile)
    (start-process "firefox" buffer "firefox" "-p" profile new-tab site)))

(defun wymux/open-document ()
  "View document."
  (interactive)
  (let ((dir "~/Media/Document")
	(reg "pdf\\|epub"))
    (start-process "llpp" "llpp" "llpp"
		   (expand-file-name
		    (completing-read "Doc: " (directory-files-recursively dir reg))))))

(customize-set-variable 'exwm-manage-configurations 
			'(((member exwm-class-name '("firefox-default" "llpp"))
			   char-mode t)))

(defun wymux/insert-gpl ()
  ""
  (interactive)
  (insert-file-contents "~/Media/Document/Archive/Reference/License/gpl3.txt"))

(customize-set-variable 'c-default-style "linux")
(customize-set-variable 'read-file-name-completion-ignore-case t)
(customize-set-variable 'read-buffer-completion-ignore-case t)
(customize-set-variable 'completion-ignore-case t)

(customize-set-variable 'backup-directory-alist '(("." . "~/Media/Document/Archive/Emacs/Edit")))
(customize-set-variable 'delete-old-versions t)
(customize-set-variable 'version-control t)
(customize-set-variable 'kept-new-versions 20)
(customize-set-variable 'kept-old-versions 20)

