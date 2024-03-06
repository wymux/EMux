(defvar wymux/eshell-ring (make-ring 1000))
(defvar wymux/eshell-history-alist nil)

(defun wymux/eshell-ug ()
  ""
  (interactive)
  (wymux/eshell-abbrev "cd ~/Internet/Git/"))

(defun wymux/eshell-umx ()
  ""
  (interactive)
  (wymux/eshell-abbrev "cd ~/Internet/Git/Exherbo/"))

(defun wymux/eshell-um ()
  ""
  (interactive)
  (wymux/eshell-abbrev "cd ~/Media/Musica"))

(defun wymux/eshell-ud ()
  ""
  (interactive)
  (wymux/eshell-abbrev "~/Media/Document/"))

(defun wymux/eshell-ugp ()
  ""
  (interactive)
  (wymux/eshell-abbrev "cd ~/Internet/Git/Project/"))

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
  "Git clone from system clipboard."
  (interactive)
  (let ((url (car kill-ring)))
    (insert (concat "git clone ")))
  (call-interactively 'yank))

(defun wymux/ums ()
  ""
  (interactive)
  (wymux/eshell-abbrev "cd ~/Internet/Git/Site/Shoash/"))

(defun wymux/eshell-pkcfa ()
  ""
  (interactive)
  (insert "packwiz curseforge add \""))

(progn
  (when (boundp 'eshell-mode-abbrev-table)
    (clear-abbrev-table eshell-mode-abbrev-table))
  (define-abbrev-table 'eshell-mode-abbrev-table
    '(("ccc" "" wymux/eshell-ccc nil)
      ("ccd" "" wymux/eshell-ccd nil)
      ("cwget" "" wymux/eshell-wget)
      ("cf" "" wymux/find-exherbo nil)
      ("cff" "" wymux/eshell-cff nil)
      ("cfp" "" wymux/eshell-cfp nil)
      ("ccs" "doas cave search --index ~/.cache/cave_all -k ")
      ("cpo" "doas cave owner")
      ("cpx" "doas cave purge")
      ("cru" nil wymux/eshell-cru nil)
      ("crw" "doas cave resolve world -cx")
      ("crx" "" wymux/eshell-crx nil)
      ("csh" "" wymux/eshell-csh nil)
      ("csl" "" wymux/exherbo-local-sync nil)
      ("csy" "doas cave sync")
      ("cvtest" "" wymux/exherbo-enable-tests)
      ("fe" "" (lambda () (call-interactively 'wymux/emms-play-find)))
      ("fp" "" (lambda () (call-interactively 'emms-play-directory-tree)))
      ("ff" "" (lambda () (call-interactively 'find-file)))
      ("fr" "" (lambda () (call-interactively 'recentf-open)) nil)
      ("gtad" "git add")
      ("gtcl" "" wymux/gtcl nil)
      ("gtcmt" "git commit -S -m \" \"")
      ("gtsc" "git switch -c")
      ("mkcd" "" wymux/eshell-mk nil)
      ("ud" "" wymux/eshell-ud nil)
      ("ug" "" wymux/eshell-ug nil)
      ("ugp" "" wymux/eshell-ugp)
      ("um" "" wymux/eshell-um nil)
      ("ums" "" wymux/ums nil)
      ("umx" "" wymux/eshell-umx nil)
      ("umxx" "" wymux/eshell-umxx nil)
      ("ro" "" wymux/open-document nil)
      ("pkcfa" "packwiz curseforge add \""  wymux/abbrev-hook-function)
      ("kil" "" wymux/eshell-kill-command))))

(progn
  (when (boundp 'emacs-lisp-mode-abbrev-table)
    (clear-abbrev-table emacs-lisp-mode-abbrev-table))
  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(("csv" "" wymux/customize-set-variable nil))))

(defun wymux/eshell-wget ()
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
    (insert (concat "wget " url))))

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

(defun wymux/eshell-abbrev (str)
  (insert str)
  (eshell-send-input))

(defun wymux/eshell-ccd ()
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg))
	(ver ""))
    (setq ver (file-name-nondirectory (read-directory-name "Version: " (format "~/Internet/Exherbo/%s/" cat/pkg))))
    (wymux/eshell-abbrev (format "cd ~/Internet/Exherbo/%s/%s" cat/pkg ver))))

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


(defun wymux/eshell-store-last-output ()
  ""
  (let ((command
	 (string-trim (buffer-substring-no-properties eshell-last-input-start eshell-last-input-end)))
	(output
	 (buffer-substring-no-properties eshell-last-input-end eshell-last-output-start)))
    (add-to-list 'wymux/eshell-history-alist `(,command . ,output))))

(add-hook 'eshell-post-command-hook 'wymux/eshell-store-last-output)

(defun wymux/eshell-kill-command ()
  ""
  (interactive)
  (let ((command (completing-read "Kill command: " wymux/eshell-history-alist))
	(str nil))
    (setq str (cdr (assoc command wymux/eshell-history-alist)))
    (kill-new (format "%s" str)))) 

(defun wymux/eshell-ccd-other-window ()
  ""
  (interactive)
  (if (= 1 (length (window-list)))
      (split-window-horizontally))
  (windmove-right)
  (eshell '99)
  (wymux/eshell-ccd)
  (eshell-send-input))

(defun wymux/eshell-csh ()
  ""
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg)))
    (insert (format "doas cave show %s" cat/pkg)))
  (eshell-send-input))

(defun wymux/eshell-cru ()
  ""
  (interactive)
  (let ((cat/pkg (wymux/exherbo-cat-pkg)))
    (insert (format "doas cave uninstall %s" cat/pkg)))
  (eshell-send-input))
