(customize-set-variable 'use-short-answers t)
(add-to-list 'find-file-not-found-functions 'wymux/create-unavailable-dir)
(add-to-list 'find-file-hook 'wymux/elevate-permission)
(customize-set-variable 'c-default-style "linux")
(customize-set-variable 'require-final-newline t)
(customize-set-variable 'disabled-command-function nil)
(customize-set-variable 'sentence-end-double-space 'nil)
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

(customize-set-variable 'undo-outer-limit 10000000000)
(customize-set-variable 'enable-recursive-minibuffers t)
