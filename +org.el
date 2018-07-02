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

   (setq +org-dir "~/Dropbox/org")

   ;; Save habit to markdown automatically
   (defun convert-habit-to-markdown ()
     (interactive)
     (when (string= (buffer-name) (file-name-nondirectory +habit-file))
       (org-md-export-to-markdown)))
   (add-hook 'after-save-hook #'convert-habit-to-markdown)

   ;; Show the entire org file automatically
   (setq org-startup-folded 'showeverything)

   ;; Set archive location
   (setq org-archive-location (concat "~/Dropbox/org/.archive/%s-" (format-time-string "%Y-%m") ".org::"))

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

   (setq org-publish-project-alist '(("org"
                                      :auto-sitemap t
                                      :base-directory "~/Dropbox/org"
                                      :base-extension "org"
                                      :html-container "div"
                                      :html-doctype "xhtml-strict"
                                      :html-extension "html"
                                      :html-preamble t
                                      :html-postamble t
                                      :html-link-use-abs-url nil
                                      :html-head-include-scripts t
                                      :html-head-include-default-style t
                                      :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.star.bris.ac.uk/bjm/css/bjm.css\" />"
                                      :org-html-html5-fancy nil
                                      :publishing-directory "~/Dropbox/org-html"
                                      :publishing-function org-html-publish-to-html
                                      :section-numbers nil
                                      :with-latex t
                                      :with-toc nil
                                      )
                                     ("md"
                                      :base-directory "~/Dropbox/org"
                                      :base-extension "md"
                                      :html-doctype "xhtml-strict"
                                      :html-extension "html"
                                      :html-preamble t
                                      :html-postamble t
                                      :html-link-use-abs-url nil
                                      :publishing-directory "~/Dropbox/org-html"
                                      :publishing-function org-md-publish-to-md
                                      :section-numbers nil
                                      :with-latex t
                                      :with-toc nil
                                      )
                                     ))

   (setq org-agenda-custom-commands
                '(("Rw" "Week in review"
                   ((agenda "" ((org-agenda-span 1)))
                    (todo "DONE" ((org-agenda-overriding-header "Done")
                                  (org-agenda-prefix-format "  * ")))
                    (todo "ACTIVE" ((org-agenda-overriding-header "Active")
                                    (org-agenda-prefix-format "  * ")))
                    (todo "TODO" ((org-agenda-overriding-header "Todo")
                                  (org-agenda-prefix-format "  * "))))

                   ((org-agenda-span 7)
                    (org-agenda-start-on-weekday 0)
                    (org-agenda-start-day "-7d")
                    (org-agenda-files '("~/Dropbox/org/work-journal.org"))
                    (org-agenda-show-all-dates t)
                    (org-agenda-compact-blocks t)
                    (org-agenda-markdown)
                    (org-agenda-remove-tags t)
                    (org-agenda-start-with-log-mode t)
                    (org-agenda-skip-archived-trees t)
                    (org-agenda-time-grid nil))

                   ("~/Dropbox/org/work-journal-summary.html"))
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
