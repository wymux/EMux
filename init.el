(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
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

(setq-default mode-line-format " %b")

(wymux/load-bin "exwm")

(wymux/load-lib "backup")
(wymux/load-lib "mode")
(wymux/load-lib "exwm")
(wymux/load-lib "recentf")
(wymux/load-lib "vertico")
(wymux/load-lib "keybind")
(wymux/load-lib "shift-number")
(wymux/load-lib "avy")
;;(wymux/load-lib "embark")
(wymux/load-lib "dired")
(wymux/load-lib "tree-sitter")
(wymux/load-lib "eshell")
(wymux/load-lib "org")
;; (wymux/load-lib "god")

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

(wymux/load-lib "abbrev")
(wymux/load-lib "mh")
(wymux/load-lib "emms")
(wymux/load-lib "register")
(wymux/load-lib "display")
;;(wymux/load-lib "eglot-java")
(wymux/load-lib "binky")

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

(wymux/load-lib "gnus")
(wymux/load-lib "ggtags")
(wymux/load-lib "eat")
(wymux/load-lib "hotfuzz")

(customize-set-variable 'c-default-style "linux")

(customize-set-variable 'require-final-newline t)

(defun wymux/go-fmt ()
  ""
  (interactive)
  (shell-command (concat "gofmt -w " (buffer-file-name)))
  (revert-buffer nil t))

(customize-set-variable 'disabled-command-function nil)
(customize-set-variable 'sentence-end-double-space 'nil)

(customize-set-variable 'eshell-scroll-show-maximum-output t)

(defun wymux/copy-filename-as-kill (&optional arg)
  ""
  (interactive)
  (if (eq arg 1)
      (kill-new (replace-regexp-in-string "\/home\/wymux" "~" (buffer-file-name)))
    (kill-new (buffer-file-name))))

(defalias 'list-buffers 'ibuffer)

(defun kill-backward-word ()
  ""
  (interactive)
  (kill-word -1))

(wymux/load-lib "wymux-keys")

(define-skeleton wymux-makefile-c
  ""
  ""
  "CFLAGS ?=\n"
  "LDFLAGS ?=\n"
  "\n"
  "PROG := " _ "\n"
  "all: $(PROG)\n"
  "\n"
  "$(PROG): $(PROG).o\n")

(customize-set-variable 'compilation-always-kill t)
(customize-set-variable 'compilation-auto-jump-to-first-error t)

(defun wymux/find-all ()
  ""
  (interactive)
  (let ((flist
	 (with-temp-buffer
	   (insert-file-contents "~/.cache/all.txt")
	   (split-string (buffer-string) "\n" t))))
    (find-file (completing-read "File: " flist))))

(defun wymux/kill-file (f)
  ""
  (interactive "fFile:")
  (with-temp-buffer
    (insert-file-contents f)
    (kill-new
     (buffer-string))))

(defun wymux/file-to-pdf ()
  ""
  (interactive)
  (let ((f (dired-get-filename nil t)))
    (shell-command (concat "libreoffice --headless --convert-to pdf " (shell-quote-argument f))))
  (revert-buffer))

(defun wymux/extract-zip ()
  ""
  (interactive)
  (let ((f (dired-get-filename nil t)))
    (shell-command (concat "unzip " (shell-quote-argument f)))
    (revert-buffer)))

(add-to-list 'load-path "~/Internet/Git/Utility/emacs-pcre")
(load-file "~/Internet/Git/Utility/hop.el/hop.el")

(customize-set-variable 'next-line-add-newlines nil)

(defun wymux/date ()
  ""
  (interactive)
  (message (format-time-string "%H:%M")))

(defun wymux/adjust-number ()
  ""
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun wymux/shift-number (n)
  ""
  (let ((old-pos (point)))
    (or (re-search-forward (rx (group (one-or-more num))) (line-end-position) t)
	(error "No number of the current line"))
    (let* ((beg		(match-beginning 1))
	   (end		(match-end	 1))
	   (old-num-str (buffer-substring-no-properties beg end))
	   (old-num	(string-to-number old-num-str))
	   (new-num	(+ old-num n))
	   (new-num-str (number-to-string new-num)))
      (delete-region beg end)
      (when (string-match-p "\\`0" old-num-str)
	(let ((len-diff (- (length old-num-str)
			   (length new-num-str))))
	  (when (> len-diff 0)
	    (insert (make-string len-diff ?0)))))
      (insert new-num-str)
      (goto-char old-pos))))

(defun wymux/shift-number-up ()
  (interactive)
  (wymux/shift-number 1))

(defun wymux/shift-number-down ()
  (interactive)
  (wymux/shift-number -1))

(defun wymux/insert-precise-time ()
  (interactive)
  (insert (concat (format-time-string "%H:%M:%S") " UTC")))

(defun wymux/insert-precise-date ()
  (interactive)
  (insert (format-time-string "%Y %d %B ")))

(defun wymux/insert-precise-date-time ()
  (interactive)
  (wymux/insert-precise-time)
  (insert " ")
  (wymux/insert-precise-date))

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer 1)
(defun wymux/video ()
  (interactive)
  (let ((v (expand-file-name
	    (completing-read "Video: " (directory-files-recursively "~/Media/Video" "")))))
    (start-process "Mpv" nil "mpv" " " v)))

(customize-set-variable 'undo-outer-limit 10000000000)

(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/emacs-w3m")
(require 'w3m-load)
(require 'mime-w3m)

;;(r equire 'lsp-bridge)
;;(global-lsp-bridge-mode)

(customize-set-variable 'lsp-bridge-enable-completion-in-minibuffer t)
(customize-set-variable 'acm-backend-lsp-candidate-min-length 3)
(customize-set-variable 'acm-backend-yas-candidate-min-length 3)
(customize-set-variable 'acm-backend-search-file-words-max-number 4)
(customize-set-variable 'acm-candidate-match-function 'hotfuzz)
(customize-set-variable 'acm-enable-quick-access t)

;;(customize-set-variable 'lsp-bridge-complete-manually t)

(defun wymux/indent-buffer ()
  ""
  (interactive)
  (indent-region (point-min) (point-max)))

(add-to-list 'load-path "~/Internet/Git/Emacs/eacl")
(add-to-list 'load-path "~/Internet/Git/Emacs/deno-bridge")
(add-to-list 'load-path "~/Internet/Git/Emacs/emmet2-mode")
(require 'emmet2-mode)
(require 'eacl)
;;(require 'lsp-java)

;; (add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/emacs-application-framework")
;; (load-file "/home/wymux/Internet/Git/Emacs/emacs-application-framework/app/browser/eaf-browser.el")
;; (require 'eaf)
;; (require 'eaf-browser)
;;(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/prettier.el")
(require 'prettier)
(add-hook 'html-mode-hook 'prettier-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company lsp-java surround prettier websocket emacs-w3m "compat" compat "compat" magit "magit"))
 '(package-vc-selected-packages
   '((lsp-java :vc-backend Git :url "https://github.com/emacs-lsp/lsp-java.git"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
