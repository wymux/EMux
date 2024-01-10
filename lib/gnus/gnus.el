(require 'gnus)

(setq gnus-select-method '(nntp "localhost"))

(customize-set-variable 'gnus-fetch-old-headers nil)

(setq-default mm-text-html-renderer 'gnus-w3m)
(setq-default w3m-safe-url-regexp nil)


(setq gnus-secondary-select-methods
      '((nntp "feedbase"
             (nntp-open-connection-function nntp-open-tls-stream) ; feedbase does not do STARTTLS (yet?)
             (nntp-port-number 563) ; nntps
             (nntp-address "feedbase.org"))))
