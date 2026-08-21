{ pkgs, ... }:

let
  packagePath = ../../../../packages/emacs;
in
{
  epkgs = epkgs: [
    epkgs.org
    (pkgs.callPackage "${packagePath}/ob-racket.nix" {
      inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
      inherit (epkgs) melpaBuild;
    })
  ];

  elisp = ''
    (use-package org
      :demand t
      :config
      (setq org-todo-keywords
            '((sequence "TODO(t@/!)"
                        "DOING(s!/!)"
                        "WAIT(w@/!)"
                        "DEFER(r!/!)"
                        "TASK(g@/!)"
                        "HABIT(h@/!)"
                        "|" "DONE(d@/!)" "CANCELED(x@/!)")
              (sequence "|" "NOTE(n)" "LINK(l)" "FEEDBACK(f)")))
      (setq org-todo-keyword-faces
            `(("TODO"     :foreground ,palette-slate-blue      :weight bold)
              ("DOING"    :foreground ,palette-yellow          :weight bold)
              ("WAIT"     :foreground ,palette-red             :weight bold)
              ("TASK"     :foreground ,palette-blue            :weight bold)
              ("DEFER"    :foreground ,palette-slate-blue-dark :weight bold)
              ("DONE"     :foreground ,palette-green-dark      :weight bold)
              ("CANCELED" :foreground "grey50"                 :weight bold :strike-through t)
              ("HABIT"    :foreground ,palette-orange          :weight bold)
              ("LINK"     :foreground ,palette-orange-dark     :weight bold)
              ("NOTE"     :foreground ,palette-red-dark        :weight bold)
              ("FEEDBACK" :foreground ,palette-purple-dark     :weight bold)))
      (setq org-todo-repeat-to-state "TODO")
      (setq org-directory "~/org/")
      (setq org-agenda-files
            (append (file-expand-wildcards (concat org-directory "agenda/*.org"))))
      (setq org-default-notes-file (concat org-directory "agenda/inbox.org"))

      (defun my/org-entry-get-immediate (property)
        (save-excursion
          (let ((local (org--property-local-values property nil)))
            (and local
                 (mapconcat #'identity
                            (delq nil local)
                            (org--property-get-separator property))))))

      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((emacs-lisp . t)
                                     (python . t)
                                     (dot . t)
                                     (scheme . t)
                                     (racket . t))))
  '';
}
