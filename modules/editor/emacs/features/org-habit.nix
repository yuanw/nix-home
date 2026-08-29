{
  elisp = ''
    (use-package org-habit
      :after org-agenda
      :config
      (setq org-habit-preceding-days 42)
      (setq org-habit-today-glyph 45)
      (custom-set-faces
       '(org-habit-alert-face ((((background light)) (:background "#f5f946"))))
       '(org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
       '(org-habit-clear-face ((((background light)) (:background "#8270f9"))))
       '(org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
       '(org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
       '(org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
       '(org-habit-ready-face ((((background light)) (:background "#4df946"))))
       '(org-habit-ready-future-face ((((background light)) (:background "#acfca9"))))))
  '';
}
