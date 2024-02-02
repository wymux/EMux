(add-to-list 'load-path "~/Internet/Git/Emacs/compat/")
(setq no-littering-etc-directory
      (expand-file-name "root/etc/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "root/var/" user-emacs-directory))
 
(add-to-list 'load-path "~/Internet/Git/Emacs/no-littering/")
(require 'no-littering)

(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

(load-file "~/.config/emacs/lib/eshell/eshell.elc")
(defvar wymux-light-theme t)

(defun wymux/dark-theme ()
  ""
  (interactive)
  (set-foreground-color "#FFFFFF")
  (set-background-color "#000000")
  (setq wymux-light-theme nil)
  (modify-all-frames-parameters '((background-color . "black")
				      (foreground-color . "white"))))

(defun wymux/bright-theme ()
  ""
  (interactive)
  (set-foreground-color "#000000")
  (set-background-color "#FFFFFF")
  (setq wymux-light-theme t)
  (modify-all-frames-parameters '((background-color . "white")
				      (foreground-color . "black"))))

(defun wymux/select-theme ()
  ""
  (let ((time-now (string-to-number (format-time-string "%H"))))
    (if (or (<= time-now 6)
	    (<= 22 time-now))
	(wymux/dark-theme)
      (wymux/bright-theme))))

(defun wymux/chromium-theme ()
  ""
  (if wymux-light-theme
      (setq browse-url-chromium-arguments '("--disable-features=WebContentsForceDark" "--user-data-dir=/home/wymux/.config/chromium/wymux-light" "--new-tab" ))
    (setq browse-url-chromium-arguments '("--enable-features=WebContentsForceDark" "--user-data-dir=/home/wymux/.config/chromium/wymux-dark" ))))

(wymux/select-theme)
(wymux/dark-theme)
(wymux/chromium-theme)
(global-font-lock-mode -1)
(electric-pair-mode 1)
(customize-set-variable 'inhibit-splash-screen t)
(add-to-list 'load-path "~/Internet/Git/Emacs/xelb")
(add-to-list 'load-path "~/Internet/Git/Emacs/exwm")

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

(defun wymux/chromium-light ()
  ""
  (start-process "Chromium" "Chromium-light" "chromium-browser" "--disable-features=WebContentsForceDark" "--user-data-dir=/home/wymux/.config/chromium/wymux-light"))

(defun wymux/chromium-dark ()
  ""
  (start-process "Chromium" "Chromium-dark" "chromium-browser"  "--enable-features=WebContentsForceDark" "--user-data-dir=/home/wymux/.config/chromium/wymux-dark"))

(defun wymux/chromium ()
  "Load chromium"
  (interactive)
  (let ((buffer "Chromium-browser"))
    (if (get-buffer buffer)
	(switch-to-buffer buffer)
      (if wymux-light-theme
	  (wymux/chromium-light)
	(wymux/chromium-dark)))))

(defun wymux/mpv ()
  "Load video."
  (interactive)
  (let ((dir "~/Media/Video")
	(reg "webm"))
    (start-process "mpv" "mpv" "mpv" (expand-file-name
				      (completing-read "Media: "
						       (directory-files-recursively dir reg))))))

(defun wymux/prismlauncher ()
  ""
  (interactive)
  (start-process "PrismLauncher" "PrismLauncher" "prismlauncher")) 

(setq exwm-input-global-keys
      `(
	([?\s-r] . exwm-reset)
	([?\s-t] . exwm-input-release-keyboard)
	([kp-end] . save-buffers-kill-emacs)
	([f1] . wymux/darken)
	([f2] . wymux/brighten)
	([?\s-w] . wymux/chromium-light)
	([kp-begin] . other-window)
	([kp-up] . split-window-vertically)
	([kp-home] . split-window-horizontally)
	([kp-left] . delete-window)
	([kp-down] . kill-this-buffer)
	([kp-delete] . wymux/mpv)
	([kp-prior] . mh-rmail)
	([kp-subtract] . gnus)
	([kp-right] . eshell)
	([kp-add] . project-eshell)
	([?\s-e] . emms)
	([?\s-s] . mh-smail)
	([?\s-d] . delete-frame)
	([?\s-u] . ffap)
	([?\s-o] . other-frame)
	([f10] . switch-to-buffer)
	([f11] . wymux/scrot-all)
	([f12] . wymux/chromium)
	([print] . wymux/scrot)
 	([kp-multiply] . previous-buffer)
	([kp-divide] . next-buffer)
	))

(require 'exwm-randr)
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DP-1-0 --above eDP-1 --mode 3440x1440 --rate 120")))
(exwm-randr-enable)

(customize-set-variable 'ediff-window-setup-function 'ediff-setup-windows-plain)

(defun wymux/customize-set-variable ()
  ""
  (interactive)
  (let ((basestr "(customize-set-variable \'")
	(var (symbol-name (read-variable "Customize: "))))
    (insert (concat basestr var " nil" ")" "\n"))))

(setq-default abbrev-mode t)

(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "C-t" 'hippie-expand)
(keymap-global-set "C-<backspace>" 'kill-whole-line)
(keymap-global-set "C-w" 'completion-at-point)
(keymap-global-set "C-i" 'dabbrev-expand)

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
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun wymux/save ()
  ""
  (if (eq major-mode 'emacs-lisp-mode)
      (elisp-byte-compile-file)))
(add-hook 'after-save-hook 'wymux/save)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/emms/")

(require 'eglot)
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


(modaled-define-state "insert"
  :lighter "[INS]"
  :cursor-type 'bar
  :no-suppress t)

(modaled-define-keys
  :states '("insert" "normal")
  :bind
  '(
    ([escape] . modaled-set-default-state)
    ([ESCAPE] . modaled-set-default-state)
    ([C-c C-h] . wymux/modaled-normal-state)
    ))

(defun wymux/modaled-qwerty ()
  ""
  (interactive)
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
      ("m" . imenu)
      ("fw" . xah-search-current-word)
      ("fb" . duplicate-dwim)
      ("fj" . dired-jump)
      
      ("xo" . wymux/format-buffer)
      ("xi" . wymux/find-exherbo)
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
      ("ff" . forward-sexp)
      ("fd" . backward-sexp)
      ("fi" . switch-to-buffer)
      ("f\\" . project-compile)
      ("f]" . project-find-file)
      ("f[" . mark-whole-buffer)
      ("fp" . ffap)
      ("fo" . occur)
      ("ft" . rgrep)
      ("fx" . exchange-point-and-mark)
      ("fu" . pop-global-mark)
      ("f'" . ispell-buffer)
      ("n" . isearch-forward)
      ("y" . yank)
      ("j" . undo)
      ("e" . backward-kill-word)
      ("r" . kill-word)
      ("c" . kill-sexp)
      ("/" . kill-whole-line)
      ("b" . kill-region)
      ("`" . delete-char)
      ("fr" . replace-string)
      ("fe" . replace-regexp)
      ("z" . recenter-top-bottom)
      ("hc" . describe-key)
      ("hf" . describe-function)
      ("hv" . describe-variable)
      ("hm" . man)
      ("haf" . apropos-function)
      ("p" . wymux/modaled-insert-state))
    
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
	("r" . dired-do-rename)
	("q" . wymux/dired-write)
	("x" . dired-do-delete)
	("u" . dired-mark)
	))

    (modaled-enable-substate-on-state-change
      "dired"
      :states '("normal")
      :major '(dired-mode))

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

    (modaled-define-substate "exheres")
    (modaled-define-keys
      :substates '("exheres")
      :bind
      '((" g" . wymux/exherbo-rename)
	(" e" . wymux/eshell-ccd-other-window)
	(" x" . wymux/exherbo-compile)))

    (modaled-enable-substate-on-state-change
      "exheres"
      :states '("normal")
      :major '(exheres-mode))    
    ))

(defun wymux/modaled-engram ()
  ""
  (interactive)
  (modaled-define-keys
    :states '("normal")
    :bind
    `(("h" . backward-char)
      ("s" . forward-char)
      ("d" . previous-line)
      ("t" . next-line)
      ("[" . backward-paragraph)
      ("]" . forward-paragraph)
      ("g" . set-mark-command)
      ("a" . execute-extended-command)
      ("," . backward-whitespace)
      ("." . forward-whitespace)
      ("b" . repeat)
      ("fm" . back-to-indentation)
      ("\'" . move-beginning-of-line)
      ("\"" . move-end-of-line)
      ("s" . mark-defun)
      ("m" . imenu)
      ("ey" . duplicate-dwim)
      ("ev" . wymux/format-buffer)
      ("eo" . find-file)
      ("el" . recentf-open)
      ("eu" . save-buffer)
      ("e[" . beginning-of-buffer)
      ("e]" . end-of-buffer)
      ("ef" . wymux/emms-play-find)
      ("ee" . emms)
      ("ed" . emms-play-directory-tree)
      ("en" . magit)
      ("e<backspace>" . compile)
      ("et" . wymux/search-www)
      ("er" . wymux/open-document)
      ("l" . backward-word)
      ("w" . forward-word)
      ("-" . forward-sexp)
      (";" . backward-sexp)
      ("ei" . switch-to-buffer)
      ("e\\" . project-compile)
      ("e_" . project-find-file)
      ("e`" . mark-whole-buffer)
      ("ep" . ffap)
      ("ef" . occur)
      ("eq" . rgrep)
      ("n" . isearch-forward)
      ("y" . yank)
      ("j" . undo)
      ("r" . backward-kill-word)
      ("m" . kill-word)
      ("c" . kill-sexp)
      ("v" . kill-whole-line)
      ("b" . kill-region)
      ("`" . delete-char)
      ("er" . replace-string)
      ("ee" . replace-regexp)
      ("z" . recenter-top-bottom)
      ("@e" . describe-function)
      ("@a" . describe-variable)
      ("@m" . man)
      ("@he" . apropos-function)
      ("v" . wymux/modaled-insert-state))))

(wymux/modaled-qwerty)

(defun wymux/exherbo-rename ()
  ""
  (interactive)
  (let ((current-file-name (file-name-nondirectory (buffer-file-name)))
	(new-file-name ""))
    (setq new-file-name (read-file-name "Bump to: " nil nil nil current-file-name))
    (rename-file current-file-name new-file-name)
    (find-file new-file-name)))

(modaled-define-default-state
  '("insert" wdired-mode eshell-mode eat-eshell-mode
    debugger-mode mh-folder-mode calendar-mode emms-playlist-mode
    magit-status-mode git-commit-mode backtrace-mode info-mode help-mode
    magit-diff-mode exwm-mode gnus-summary-mode gnus-group-mode-hook
    text-mode magit-refs-mode magit-select-mode magit-log-mode
    gnus-group-mode)
  '("normal"))

(defun wymux/modaled-insert-state ()
  ""
  (interactive)
  (modaled-set-state "insert"))

(defun wymux/modaled-normal-state ()
  ""
  (interactive)
  (modaled-set-state "normal"))

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
			'(((member exwm-class-name '("llpp" "Chromium-browser"))
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

(defun wymux/find-exherbo ()
  ""
  (interactive)
  (let ((exherbo-file (completing-read "Exherbo file: "
				       (directory-files-recursively "~/Internet/Git/Exherbo/"
								    "exheres-0$\\|exlib$"))))
    (find-file exherbo-file)))

(defun wymux/doas ()
  ""
  (interactive)
  (when (not (file-writable-p buffer-file-name))
    (progn
      (find-alternate-file (concat "/doas::" buffer-file-name))
      (read-only-mode -1))))

(add-hook 'find-file-hook 'wymux/doas)

(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 20)
(setq completion-auto-select t)
(setq completion-auto-help 'visible)

(defun wymux/exherbo-local-sync ()
  "cave sync -s local/`repo' -r origin/`package'"
  (interactive)
  (let ((rep (file-name-nondirectory
	      (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
	(pkg (string-trim (car (vc-git-branches))))
	(cmd ""))
    (setq cmd (format "doas cave sync -s local %s -r origin/%s" rep pkg))
    (insert cmd)))

(defun wymux/exherbo-enable-tests ()
  "Enable tests"
  (interactive)
  (let ((tests "BUILD_OPTIONS: recommended_tests")
	(cat/pkg (read-from-minibuffer "Cat/Pkg: "))
	(cmd ""))
    (setq cmd (format "echo %s %s >> /etc/paludis/options.conf" cat/pkg tests))
    (with-temp-buffer
      (cd "/doas::/")
      (async-shell-command cmd))))

(customize-set-variable 'eshell-list-files-after-cd t)

(defun wymux/scrot ()
  "Screenshot"
  (interactive)
  (let ((flags "-q 100 -s")
	(date (format-time-string "%m-%d-%Y-%H-%M-%S-%N-%6N-%3N")))
    (shell-command (concat "scrot -q 100 -s /tmp/" date ".png"))))

(defun wymux/scrot-all ()
  "Screenshot."
  (interactive)
  (let ((date (format-time-string "%m-%d-%Y-%H-%M-%S-%N-%6N-%3N")))
    (shell-command (concat "scrot  -q 100 /tmp/" date ".png"))))

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(setq gnus-select-method '(nntp "localhost"))

(customize-set-variable 'gnus-fetch-old-headers nil)

(setq-default mm-text-html-renderer 'gnus-w3m)
(setq-default w3m-safe-url-regexp nil)

(defun wymux/auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'wymux/auto-create-missing-dirs)

(customize-set-variable 'disabled-command-function nil)

(setq-default ido-mode nil)

(defun wymux/dired-write ()
  ""
  (interactive)
  (modaled-dired-substate-mode -1)
  (modaled-insert-state-mode -1)
  (wdired-change-to-wdired-mode))

(defun wymux/wdired-finish-edit ()
  ""
  (interactive)
  (wdired-finish-edit)
  (modaled-dired-substate-mode 1)
  (modaled-insert-state-mode 1))

(require 'wdired)
(keymap-set wdired-mode-map "C-c C-c" 'wymux/wdired-finish-edit)
(setq browse-url-browser-function 'browse-url-chromium)

(customize-set-variable 'save-interprogram-paste-before-kill t)
(customize-set-variable 'yank-pop-change-selection t)
(setq select-enable-clipboard t)

(defun magit-display-buffer-pop-up-frame (buffer)
  (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-frame)
                        (reusable-frames . t)))
    (magit-display-buffer-traditional buffer)))

(setq magit-display-buffer-function #'magit-display-buffer-pop-up-frame)
(customize-set-variable 'exwm-workspace-number 1)

(defun wymux/get-weblinks ()
  "User selects link from current buffer."
  (interactive)
  (let (url-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "https.+" nil t)
	(push
	 (buffer-substring-no-properties (match-beginning 0) (match-end 0))
	 url-list)))
    (browse-url (completing-read "Goto: " url-list))))

(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( -p1 -p2 )
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq -p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq -p2 (point))))
    (setq mark-active nil)
    (when (< -p1 (point))
      (goto-char -p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties -p1 -p2))))

(defun wymux/insert-exherbo-cat-pkg ()
  ""
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg)))
    (insert cat/pkg)))

(keymap-global-set "C-c C-e" 'wymux/insert-exherbo-cat-pkg)

(defun wymux/exherbo-cee ()
  "cmake options"
  (interactive)
  (let ((option (read-from-minibuffer "OPTION: "))
	(boolean (completing-read "BOOLEAN: " '("FALSE" "TRUE"))))
    (insert (format "-D%s:BOOL=%s" option boolean))))

(progn
  (when (boundp 'exheres-mode-abbrev-table)
    (clear-abbrev-table exheres-mode-abbrev-table))
  (define-abbrev-table 'exheres-mode-abbrev-table
    '(("ee" "--enable-")
      ("dd" "--disable")
      ("ww" "--with")
      ("wo" "--without")
      ("cee" "" exherbo-cee)
      ("cscb" "CMAKE_SRC_CONFIGURE_OPTION_BUILDS")
      ("csce" "CMAKE_SRC_CONFIGURE_OPTION_ENABLES")
      ("cscp" "CMAKE_SRC_CONFIGURE_PARAMS")
      ("cscp" "CMAKE_SRC_CONFIGURE_OPTIONS")
      ("csct" "CMAKE_SRC_CONFIGURE_TESTS")
      ("cscw" "CMAKE_SRC_CONFIGURE_OPTION_WANTS")
      ("cscwi" "CMAKE_SRC_CONFIGURE_OPTION_WITHS")
      ("csip" "CMAKE_SRC_INSTALL_PARAMS")
      ("mscf" "MESON_SRC_CONFIGURE_OPTION_FEATURES")
      ("msco" "MESON_SRC_CONFIGURE_OPTIONS")
      ("mscp" "MESON_SRC_CONFIGURE_PARAMS")
      ("mscs" "MESON_SRC_CONFIGURE_OPTION_SWITCHES")
      ("msct" "MESON_SRC_CONFIGURE_TESTS")
      ("ccc" "" wymux/insert-exherbo-cat-pkg))))

(progn
  (when (boundp 'exlib-mode-abbrev-table)
    (clear-abbrev-table exlib-mode-abbrev-table))
  (define-abbrev-table 'exlib-mode-abbrev-table
    '(("ee" "--enable-")
      ("dd" "--disable")
      ("ww" "--with")
      ("wo" "--without")
      ("cee" "" exherbo-cee)
      ("cscb" "CMAKE_SRC_CONFIGURE_OPTION_BUILDS")
      ("csce" "CMAKE_SRC_CONFIGURE_OPTION_ENABLES")
      ("cscp" "CMAKE_SRC_CONFIGURE_PARAMS")
      ("cscp" "CMAKE_SRC_CONFIGURE_OPTIONS")
      ("csct" "CMAKE_SRC_CONFIGURE_TESTS")
      ("cscw" "CMAKE_SRC_CONFIGURE_OPTION_WANTS")
      ("cscwi" "CMAKE_SRC_CONFIGURE_OPTION_WITHS")
      ("csip" "CMAKE_SRC_INSTALL_PARAMS")
      ("mscf" "MESON_SRC_CONFIGURE_OPTION_FEATURES")
      ("msco" "MESON_SRC_CONFIGURE_OPTIONS")
      ("mscp" "MESON_SRC_CONFIGURE_PARAMS")
      ("mscs" "MESON_SRC_CONFIGURE_OPTION_SWITCHES")
      ("msct" "MESON_SRC_CONFIGURE_TESTS")
      ("ccc" "" wymux/insert-exherbo-cat-pkg))))

(defun wymux/exherbo-compile ()
  ""
  (interactive)
  (project-eshell)
  (eshell/clear)
  (wymux/exherbo-local-sync)
  (insert " && ")
  (insert (format "doas cave resolve -x %s" (car (vc-git-branches))))
  (eshell-send-input))
