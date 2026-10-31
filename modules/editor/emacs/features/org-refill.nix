{
  elisp = ''
    (defvar my/org-refill-max-level 2
      "Maximum headline level `my/org-refill' offers as a target.")

    (defun my/org-refill-visible-org-files ()
      "Return file names of Org buffers shown outside the selected window."
      (let ((selected (selected-window))
            files)
        (dolist (frame (frame-list))
          (dolist (window (window-list frame 'no-minibuf))
            (unless (eq window selected)
              (let* ((buffer (window-buffer window))
                     (base (or (buffer-base-buffer buffer) buffer))
                     (file (buffer-file-name base)))
                (when (and file
                           (eq (buffer-local-value 'major-mode base) 'org-mode))
                  (push file files))))))
        files))

    (defun my/org-refill-target-files ()
      "Files `my/org-refill' looks in: the agenda files plus Org buffers shown in other windows."
      (cl-remove-duplicates (append (org-agenda-files 'unrestricted)
                                    (my/org-refill-visible-org-files))
                            :test #'equal))

    (defun my/org-refill (&optional arg)
      "Refile the entry at point to the top of a target instead of the end.  Targets are headings up to `my/org-refill-max-level' in `my/org-refill-target-files'; prefix ARG works as in `org-refile'."
      (interactive "P")
      (let ((org-refile-targets
             `((my/org-refill-target-files :maxlevel . ,my/org-refill-max-level)))
            (org-refile-use-outline-path 'file)
            (org-reverse-note-order t))
        (org-refile arg)))

    (use-package org
      :bind (:map org-mode-map
             ("C-c C-w" . my/org-refill)
             ("C-c C-M-w" . org-refile)))
  '';
}
