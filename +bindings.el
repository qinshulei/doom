;;; +bindings.el --- description -*- lexical-binding: t; -*-

(map!
 "C-+"   'text-scale-increase
 "C--"   'text-scale-decrease
 :n "-"  'dired-jump
 "M-B"   'recompile

 (:leader :n "d" 'deft)
 (:leader :n "r" 'counsel-load-theme))

(map! :after neotree
      :map neotree-mode-map
      :n "M-RET" #'neotree-enter-ace-window)

(provide '+bindings)

;;; +bindings.el ends here
