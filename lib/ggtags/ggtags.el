(load-file "~/Internet/Git/Emacs/ggtags/ggtags.elc")

(defun wymux/ggtags ()
  ""
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)))

(add-hook 'c-mode-comm2on-hook 'wymux/ggtags)

