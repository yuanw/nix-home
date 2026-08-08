{
  elisp = ''
    (use-package org-agenda
      :after org
      :defer t
      :bind (("C-c a" . org-agenda))
      :config
      (setq org-agenda-span 5
            org-deadline-warning-days 14
            org-agenda-show-all-dates t
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-scheduled-if-done t
            org-agenda-start-on-weekday nil)
      (setq org-agenda-custom-commands
            '(("d" "Daily agenda"
               ((agenda "" ((org-agenda-span 'day)
                            (org-agenda-overriding-header "Today's Agenda")))
                (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
                (todo "WAITING" ((org-agenda-overriding-header "Waiting On")))))
              ("w" "Weekly review"
               ((agenda "" ((org-agenda-span 'week)
                            (org-agenda-overriding-header "Weekly Overview")))
                (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
                (todo "TODO" ((org-agenda-overriding-header "All TODOs")))))
              ("p" "Projects"
               ((tags-todo "+LEVEL=2/TODO"
                           ((org-agenda-overriding-header "Active Projects")))))
              ("n" "Next actions"
               ((todo "NEXT" ((org-agenda-overriding-header "All Next Actions")))))
              ("W" "Waiting"
               ((todo "WAITING" ((org-agenda-overriding-header "All Waiting Items"))))))))
  '';
}
