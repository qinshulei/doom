;;; +custom.el --- description -*- lexical-binding: t; -*-

(after! deft
  (setq deft-extensions '("md" "md.gpg" "org")
        deft-default-extension "md"
        deft-directory "~/Dropbox/Notes"
        deft-use-filename-as-title t
        deft-auto-save-interval 15.0
        deft-text-mode 'markdown-mode
        deft-recursive t))

(def-package! deft
  :commands  (deft))

(def-package! groovy-mode
  :commands  (groovy-mode))

(setq +workspaces-switch-project-function #'dired)

(whitespace-mode nil)

(provide '+custom)

;;; +custom.el ends here
