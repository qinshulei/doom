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
(setq auth-sources '("~/.authinfo.gpg"))

(after! easy-hugo
  (setq easy-hugo-basedir "~/work/blog2/")
  (setq easy-hugo-postdir "~/work/blog2/content/posts")
  (setq easy-hugo-markdown-extension "md"))

(whitespace-mode nil)

(after! tide
        (set! :company-backend 'tide-mode 'company-flow 'company-tide))

(defun setup-prettier-js ()
  "Sets up arguments and the mode."
  (interactive)
  (setq prettier-js-args '("--single-quote"))
  (prettier-js-mode))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename matches the regexp.
  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(after! web-mode
        (add-hook! web-mode (enable-minor-mode '("\\.tsx\\'" . setup-prettier-js))))

(after! typescript-mode
        (defun tslint-fix-file ()
          "Tslint fix file."
          (interactive)
          (message (concat "tslint --fixing the file " (buffer-file-name)))
          (shell-command (concat "tslint --fix " (buffer-file-name))))

        (defun tslint-fix-file-and-revert ()
          "Format the current file with Tslint."
          (interactive)
          (when (eq major-mode 'typescript-mode)
            (if (executable-find "tslint")
              (tslint-fix-file)
              (message "Tslint not found."))))

        (add-hook 'after-save-hook #'tslint-fix-file-and-revert)
        (add-hook 'typescript-mode-hook #'flycheck-mode)
        ;; Prettier shit
        (add-hook 'typescript-mode-hook #'setup-prettier-js)
        (setq typescript-indent-level 2))

(after! js2-mode
        ;; use eslintd-fix so when i save it fixes dumb shit
        (add-hook 'js2-mode-hook #'eslintd-fix-mode)

        ;; FLOW STUFF
        (add-hook 'js2-mode-hook #'flow-minor-enable-automatically)

        ;; Prettier shit
        (add-hook 'js2-mode-hook #'setup-prettier-js)

        ;; Indent shit
        (setq js2-basic-offset 2))

(after! web-mode
        (add-hook 'web-mode-hook #'flycheck-mode)

        (setq web-mode-markup-indent-offset 2 ;; Indentation
              web-mode-code-indent-offset 2
              web-mode-enable-auto-quoting nil ;; disbale adding "" after an =
              web-mode-auto-close-style 2)) ;; Close on > and </ not just </

(after! elm
        (setq elm-tags-on-save t
              elm-sort-imports-on-save t))

(after! circe
  (circe-set-display-handler "354" 'circe-display-ignore))

(set! :irc "shara.xen.prgmr.com"
  `(:tls t
          :nick "rphillips"
          :host "71.19.146.50"
          :port 32321
          :user ,(+pass-get-user "irc/freenode")
          :pass (lambda (&rest _) (+pass-get-secret "irc/freenode"))))

;; **********************************************************************
;; Dired

(defun rphillips/dired-setup ()
  "Setup custom dired settings for vinegar"
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)

  ;; hide details by default
  (dired-hide-details-mode t)
  )

(add-hook 'dired-mode-hook 'rphillips/dired-setup)

;; Whitespace tweak for go
(remove-hook 'after-change-major-mode-hook #'doom|show-whitespace-maybe)

(provide '+custom)

;;; +custom.el ends here
