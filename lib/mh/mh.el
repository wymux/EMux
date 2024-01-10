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
