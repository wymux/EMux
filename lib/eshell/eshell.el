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

(defun wymux/eshell-umxx ()
  ""
  (interactive)
  (let ((repo (completing-read "Track: " (directory-files "~/Internet/Git/Exherbo/"))))
    (insert (concat "cd ~/Internet/Git/Exherbo/" repo)))
  (eshell-send-input))

(defun wymux/eshell-cff ()
  "Find exherbo outdated file."
  (interactive)
  (find-file "~/.cache/ex-outdated.json"))

(defun wymux/eshell-cfp ()
  "Find exherbo problem file."
  (interactive)
  (find-file "~/.cache/ex-problems.json"))

(defun wymux/gtcl ()
  "Git clone from clipboard."
  (interactive)
  (let ((url (car kill-ring)))
    (insert (concat "git clone " url))))

(defun wymux/ums ()
  ""
  (interactive)
  (insert "cd ~/Internet/Git/Site/Shoash/")
  (eshell-send-input))

(progn
  (when (boundp 'eshell-mode-abbrev-table)
    (clear-abbrev-table eshell-mode-abbrev-table))
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("ccc" "" wymux/eshell-ccc nil)
      ("ccd" "" wymux/eshell-ccd nil)
      ("ccurl" "" wymux/eshell-curl)
      ("cf" "" wymux/find-exherbo nil)
      ("cff" "" wymux/eshell-cff nil)
      ("cfp" "" wymux/eshell-cfp nil)
      ("css" "doas cave search")
      ("cpo" "doas cave owner")
      ("cpx" "doas cave purge")
      ("cru" "doas cave uninstall")
      ("crw" "doas cave resolve world -cx")
      ("crx" "" wymux/eshell-crx nil)
      ("csh" "doas cave show")
      ("csl" "" wymux/exherbo-local-sync nil)
      ("csy" "doas cave sync")
      ("cvtest" "" wymux/exherbo-enable-tests)
      ("gtad" "git add")
      ("gtcl" "" wymux/gtcl nil)
      ("gtcmt" "git commit -S -m \" \"")
      ("gtsc" "git switch -c")
      ("mk" "" wymux/eshell-mk nil)
      ("ug" "" wymux/eshell-ug nil)
      ("ugp" "" wymux/eshell-ugp)
      ("um" "" wymux/eshell-um nil)
      ("ums" "" wymux/ums nil)
      ("umx" "" wymux/eshell-umx nil)
      ("umxx" "" wymux/eshell-umxx nil))))

(progn
  (when (boundp 'emacs-lisp-mode-abbrev-table)
    (clear-abbrev-table emacs-lisp-mode-abbrev-table))
  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(("csv" "" wymux/customize-set-variable nil))))

(defun wymux/eshell-curl ()
  ""
  (interactive)
  (let ((url "")
	(cat/pkg (replace-regexp-in-string
	      "/home/wymux/Internet/Exherbo/" "" (completing-read "cat/pkg: " (split-string (shell-command-to-string "find ~/Internet/Exherbo -type d -maxdepth 2 -mindepth 2"))))))
    (with-temp-buffer
    (shell-command (concat "doas cave show -n -k DOWNLOADS " cat/pkg) (current-buffer))
      (goto-char (point-min))
      (search-forward "Down")
      (search-forward "://")
      (setq url (thing-at-point 'url)))
      (insert (concat "wget -O " url))))

(defun wymux/eshell-crx ()
  ""
  (interactive)
  (let ((cat/pkg (replace-regexp-in-string
		  "/home/wymux/Internet/Exherbo/" "" (completing-read "cat/pkg: " (split-string (shell-command-to-string "find ~/Internet/Exherbo -type d -maxdepth 2 -mindepth 2"))))))
    (insert (concat "doas cave resolve -x " cat/pkg)))
    (eshell-send-input))

(defun wymux/eshell-mk ()
  "mkdir and cd"
  (interactive)
  (let ((dir (read-from-minibuffer "Dir: ")))
    (insert (concat "mkdir " dir " && cd " dir))
    (eshell-send-input)))

(defun wymux/eshell-ccc ()
  ""
  (interactive)
  (let ((cat/pkg (replace-regexp-in-string
		  "/home/wymux/Internet/Exherbo/" "" (completing-read "cat/pkg: " (split-string (shell-command-to-string "find ~/Internet/Exherbo -type d -maxdepth 2 -mindepth 2"))))))
    (insert (concat "doas cave contents " cat/pkg)))
  (eshell-send-input))

(defun wymux/exherbo-cat-pkg ()
  ""
  (interactive)
    (replace-regexp-in-string
     "/home/wymux/Internet/Exherbo/" "" (completing-read "cat/pkg: " (split-string (shell-command-to-string "find ~/Internet/Exherbo -type d -maxdepth 2 -mindepth 2")))))

(defun wymux/eshell-ccd ()
  ""
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg)))
    (insert (concat "cd ~/Internet/Exherbo/" cat/pkg)))
  (eshell-send-input))

(defun wymux/eshell-copt ()
  ""
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg))
	(exheres-dir "")
	(exheres "")
	(options ""))
    (setq exheres-dir (concat (string-trim (shell-command-to-string
		       (format "find ~/Internet/Git/Exherbo/ -type d -wholename *%s*" cat/pkg))) "/"))
    (setq exheres (read-file-name "Exheres: " exheres-dir))
    (setq options (shell-command-to-string (concat "ex-option.sh " exheres)))
    (kill-new options)))

(setq eshell-prompt-function
  (lambda ()
    (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
      (if (= (user-uid) 0) " # " " $ "))))

