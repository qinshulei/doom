;;; +bindings.el --- description -*- lexical-binding: t; -*-

(map!
 "C-+"   'text-scale-increase
 "C--"   'text-scale-decrease
 :n "-"  'dired-jump
 "M-B"   'recompile

 (:prefix "C-c v"
   "m" 'magit-status)

 (:leader :n "d" 'deft))

(provide '+bindings)

;;; +bindings.el ends here
