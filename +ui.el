;;; +ui.el --- description -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-use-proxy-icon nil)

(setq doom-font (font-spec :family "Iosevka" :size 13))
(setq doom-big-font (font-spec :family "Iosevka" :size 26))
(setq doom-molokai-brighter-comments t)
(setq doom-theme 'doom-one-light)

(unless (display-graphic-p)
  (setq doom-theme 'doom-one)
  (set-face-background 'default "black")
  (set-face-background 'region "black")
  (set-face-foreground 'default "white")
  (set-face-foreground 'region "gray60")
  (set-background-color "black"))

(after! posframe
  (add-variable-watcher
   'posframe--frame
   (lambda (_sym frame op _where)
     (when (and (eq op 'set) frame)
       (with-selected-frame frame
         (setq-local whitespace-style nil))))))

(setq show-trailing-whitespace nil)

(after! neotree
  (setq-default neo-smart-open t)
  (setq doom-neotree-enable-variable-pitch nil)
  (setq neo-theme (if window-system 'icons 'nerd))
  (setq neo-show-hidden-files nil)
  (setq neo-autorefresh t)
  (set-face-attribute 'neo-vc-edited-face nil
                      :foreground "#E2C08D")
  (set-face-attribute 'neo-vc-added-face nil
                      :foreground "green4"))

(def-package! resize-window
  :commands  (resize-window))

(provide '+ui)

;;; +ui.el ends here
