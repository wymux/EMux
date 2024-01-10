(add-to-list 'load-path "/home/wymux/Internet/Git/Emacs/emacs-w3m")
(add-to-list 'load-path "~/Internet/Git/Emacs/eacl")
(add-to-list 'load-path "~/Internet/Git/Emacs/deno-bridge")
(add-to-list 'load-path "~/Internet/Git/Emacs/emmet2-mode")
(add-to-list 'load-path "~/Internet/Git/Emacs/prettier.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/emacs-websocket/")
(add-to-list 'load-path "~/Internet/Git/Emacs/iter2/")
(add-to-list 'load-path "~/Internet/Git/Emacs/s.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/f.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/editorconfig-emacs/")
(add-to-list 'load-path "~/Internet/Git/Emacs/nvm.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/with-editor/lisp/")
(add-to-list 'load-path "~/Internet/Git/Emacs/dash.el/")
(add-to-list 'load-path "~/Internet/Git/Emacs/transient/")
(add-to-list 'load-path "~/Internet/Git/Emacs/magit/lisp/")

(require 'package)
(require 's)
(require 'f)
(require 'nvm)
(require 'editorconfig)
(require 'prettier)
(require 'eacl)
(require 'emmet2-mode)
(require 'mime-w3m)
(require 'w3m-load)
(require 'dash)
(require 'with-editor)
(require 'magit)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(customize-set-variable 'package-install-upgrade-built-in t)
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

(wymux/load-lib "abbrev")
(wymux/load-lib "backup")
(wymux/load-lib "dired")
(wymux/load-lib "display")
(wymux/load-lib "eat")
(wymux/load-lib "emms")
(wymux/load-lib "eshell")
(wymux/load-lib "exwm")
(wymux/load-lib "ggtags")
(wymux/load-lib "gnus")
(wymux/load-lib "hotfuzz")
(wymux/load-lib "keybind")
(wymux/load-lib "mh")
(wymux/load-lib "mode")
(wymux/load-lib "hexrgb")
(wymux/load-lib "oneonone")
(wymux/load-lib "recentf")
(wymux/load-lib "register")
(wymux/load-lib "tree-sitter")
(wymux/load-lib "vertico")

(customize-set-variable 'use-short-answers t)

(add-to-list 'find-file-not-found-functions 'wymux/create-unavailable-dir)

(add-to-list 'find-file-hook 'wymux/elevate-permission)

(customize-set-variable 'c-default-style "linux")

(customize-set-variable 'require-final-newline t)

(customize-set-variable 'disabled-command-function nil)
(customize-set-variable 'sentence-end-double-space 'nil)

(customize-set-variable 'eshell-scroll-show-maximum-output t)

(defalias 'list-buffers 'ibuffer)

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

(add-to-list 'load-path "~/Internet/Git/Utility/emacs-pcre")
(load-file "~/Internet/Git/Utility/hop.el/hop.el")

(customize-set-variable 'next-line-add-newlines nil)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-kill-when-opening-new-dired-buffer 1)

(customize-set-variable 'undo-outer-limit 10000000000)


(add-hook 'html-mode-hook 'prettier-mode)
(add-hook 'css-mode-hook 'prettier-mode)
(add-hook 'js-mode-hook 'prettier-mode)

(customize-set-variable 'enable-recursive-minibuffers t)
(customize-set-variable 'completion-ignore-case t)
(customize-set-variable 'read-file-name-completion-ignore-case t)
(customize-set-variable 'read-buffer-completion-ignore-case t)

