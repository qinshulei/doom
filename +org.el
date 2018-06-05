;;;  -*- lexical-binding: t; -*-
(setq +todo-file "~/Dropbox/org/todo.org")
(setq +habit-file "~/Dropbox/org/habit.org")
(setq +work-journal "~/Dropbox/org/work-journal.org")

(after! org (require 'org-agenda)
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
          ("w" "Work" entry (file+headline "~/Dropbox/org/work-journal.org" "Work")
           "* %?\n%i" :prepend t :kill-buffer t)
          ))

   ;;(setq org-agenda-custom-commands
   ;;      '(("wS" "Export Schedule" (
   ;;                                 (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!"
   ;;                                            ((org-agenda-overriding-header "Active:")))
   ;;                                 (tags-todo "-INACTIVE-HOLD-CANCELLED-REFILE-ARCHIVEr/!-CLOSED"
   ;;                                            ((org-agenda-overriding-header "Done:"))))
   ;;         ((org-agenda-start-with-log-mode t)
   ;;          (org-agenda-compact-blocks t)
   ;;          (org-agenda-log-mode-items '(clock))
   ;;          (org-agenda-todo-ignore-deadlines 'near)
   ;;          (org-agenda-todo-ignore-scheduled t)) ("~/Dropbox/org/work-journal-summary.html"))
   ;;        ("X" "Agenda" ((agenda "") (alltodo))
   ;;         ((org-agenda-ndays 10)
   ;;          (org-agenda-start-on-weekday nil)
   ;;          (org-agenda-start-day "-3d")
   ;;          (org-agenda-start-with-log-mode t)
   ;;          (org-agenda-log-mode-items '(closed clock state)))
   ;;         )))


   (add-to-list 'org-agenda-custom-commands
                '("R" . "Review" ))

   (setq org-agenda-custom-commands
      '(("P" "Printed agenda"
         ((agenda "" ((org-agenda-span 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday nil)         ;; calendar begins today
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (agenda "" ((org-agenda-span 1)                      ; daily agenda
                      (org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
           (ps-landscape-mode t))
         ("~/agenda.ps"))
        ;; other commands go here
        ))

   (setq +agenda-date-str (format ""))

   (setq org-agenda-custom-commands
                '(("Rw" "Week in review"
                   ((todo "DONE"
                          ((org-agenda-overriding-header "Finished")
                           (org-agenda-prefix-format "  * ")))
                    (todo "ACTIVE"
                          ((org-agenda-overriding-header "Active"))
                          (org-agenda-prefix-format "  * "))
                    (todo "TODO"
                          ((org-agenda-overriding-header "Todo")
                           (org-agenda-prefix-format "  * "))))

                   ((org-agenda-span '10)
                    (org-agenda-start-on-weekday 0)
                    (org-agenda-start-day "-7d")
                    (org-agenda-files '("~/Dropbox/org/work-journal.org"))
                    (org-agenda-show-all-dates t)
                    (org-agenda-compact-blocks t)
                    (org-agenda-remove-tags t)
                    (org-agenda-start-with-log-mode t)
                    (org-agenda-archives-mode t)
                    (org-agenda-time-grid nil))
                   ("~/Dropbox/org/week-journal-summary.html"))
                  ;; other commands go here
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

(defun +open-work-journal ()
  (interactive)
  "Opens the work journal"
  (find-file +work-journal))

(map!
 (:leader
   :desc "Open work journal" :nvm "W" #'+open-work-journal
   :desc "Open todo file" :nvm "O" #'+open-todo-file
   :desc "Open habit file" :nvm "H" #'+open-habit-file))

(map! :leader
      (:prefix "o"
        :nvm "a" (lambda! (org-agenda nil "a"))))

(provide '+org)
