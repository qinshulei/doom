;;; +ui.el --- description -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-use-proxy-icon nil)

(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-big-font (font-spec :family "Fira Code" :size 22))
(setq doom-molokai-brighter-comments t)
(setq doom-theme 'doom-opera)

(unless (display-graphic-p)
  (setq doom-theme nil))

(after! posframe
  (add-variable-watcher
   'posframe--frame
   (lambda (_sym frame op _where)
     (when (and (eq op 'set) frame)
       (with-selected-frame frame
         (setq-local whitespace-style nil))))))

;; Magit rules
;; Keeps it on the side
;; Thanks to https://github.com/fuxialexander/doom-emacs-private-xfu
(after! magit
  (set! :popup "^\\(?: ?\\*\\)?magit.*: "
    '((slot . -1) (side . right) (size . 80))
    '((select . t) (quit . nil)))
  (set! :popup "^\\*magit.*popup\\*"
    '((slot . 0) (side . right))
    '((select . t)))
  (set! :popup "^\\(?: ?\\*\\)?magit-revision:.*"
    '((slot . 2) (side . right) (window-height . 0.6))
    '((select . t)))
  (set! :popup "^\\(?: ?\\*\\)?magit-diff:.*"
    '((slot . 2) (side . right) (window-height . 0.6))
    '((select . nil))))

(setq show-trailing-whitespace nil)

(after! neotree
  (setq-default neo-smart-open t)
  (setq doom-neotree-enable-variable-pitch nil)
  (setq neo-theme (if window-system 'icons 'nerd))
  (setq neo-show-hidden-files nil)
  (setq neo-autorefresh t)
  ;;(setq neo-vc-integration '(face char))
  (set-face-attribute 'neo-vc-edited-face nil
                      :foreground "#E2C08D")
  (set-face-attribute 'neo-vc-added-face nil
                      :foreground "green4"))

(def-package! resize-window
  :commands  (resize-window))

(provide '+ui)

;;; +ui.el ends here
