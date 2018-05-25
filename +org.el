;;;  -*- lexical-binding: t; -*-
(setq +todo-file "~/Dropbox/org/todo.org")
(setq +habit-file "~/Dropbox/org/habit.org")

(after! org
   (map! :map evil-org-mode-map
         :localleader
         :desc "Create/Edit Todo" :nve "o" #'org-todo
         :desc "Schedule"         :nve "s" #'org-schedule
         :desc "Deadline"         :nve "d" #'org-deadline
         :desc "Refile"           :nve "r" #'org-refile
         :desc "Filter"           :nve "f" #'org-match-sparse-tree
         :desc "Tag heading"      :nve "t" #'org-set-tags-command)

   ;; Save habit to markdown automatically
   (defun convert-habit-to-markdown ()
     (interactive)
     (when (string= (buffer-name) (file-name-nondirectory +habit-file))
       (org-md-export-to-markdown)))
   (add-hook 'after-save-hook #'convert-habit-to-markdown)

   ;; Show the entire org file automatically
   (setq org-startup-folded 'showeverything)

   ;; Set archive location
   (setq org-archive-location (concat "~/Dropbox/org/.archive/" (format-time-string "%Y-%m") ".org::"))

   ;; Org capture template
   (setq org-capture-templates
        '(("t" "Inbox" entry (file+headline "~/Dropbox/org/inbox.org" "Inbox")
           "* %?\n  %i\n  %a")
          ("h" "Habit" entry (file+headline "~/Dropbox/org/habit.org" "Habit")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("n" "Notes" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("wk" "Work Kubernetes" entry (file+headline "~/Dropbox/org/work-kubernetes.org" "Work Kubernetes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("wo" "Work Openshift" entry (file+headline "~/Dropbox/org/work-openshift.org" "Work Openshift")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ))

   (setq org-agenda-files (quote ("~/Dropbox/org")))
   (setq org-modules (cons 'org-habit org-modules))
   (setq org-outline-path-complete-in-steps nil)
   (setq org-refile-targets
         (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 2))))
   (setq org-refile-allow-creating-parent-nodes 'confirm)

   ;; Normally its only like 3 lines tall, too hard to see anything.
   (set! :popup "^\\*Org Agenda"
     '((size . 15))
     '((transient) (quit) (select . t))))

;; org-match-sparse-tree
;; org-set-tags-command
(defun +open-todo-file ()
  (interactive)
  "Opens the todo file"
  (find-file +todo-file))

(defun +open-habit-file ()
  (interactive)
  "Opens the habit file"
  (find-file +habit-file))

(map!
 (:leader
   :desc "Open todo file" :nvm "O" #'+open-todo-file
   :desc "Open habit file" :nvm "H" #'+open-habit-file))

(map! :leader
      (:prefix "o"
        :nvm "a" (lambda! (org-agenda nil "a")))
      (:when (featurep! :completion helm)
        :nv "X" #'helm-org-capture-templates))

(provide '+org)
