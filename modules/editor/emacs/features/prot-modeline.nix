{ ... }:

{
  epkgs = epkgs: [
    epkgs.keycast
    (epkgs.trivialBuild {
      pname = "prot-common";
      version = "0.0.1";
      src = ../packages/prot-common.el;
    })
    (epkgs.trivialBuild {
      pname = "prot-modeline";
      version = "0.0.1";
      src = ../packages/prot-modeline.el;
      packageRequires = [
        (epkgs.trivialBuild {
          pname = "prot-common";
          version = "0.0.1";
          src = ../packages/prot-common.el;
        })
      ];
    })
  ];

  elisp = ''
    ;; nima concatenates feature Elisp directly, unlike the old Home Manager
    ;; `use-package' generator.  Require the local package first so the
    ;; prot-modeline faces, variables, and VC helpers are defined before we set
    ;; `mode-line-format'.  Without this, the modeline can miss colors and git
    ;; branch information.
    (require 'prot-modeline)

    ${builtins.readFile ./prot-modeline-config.el}

    ;; Display current time/date through `mode-line-misc-info', consumed by
    ;; `prot-modeline-misc-info' on the right side of the modeline.
    (use-package time
      :ensure nil
      :hook (after-init . display-time-mode)
      :config
      (setq display-time-format " %a %e %b, %H:%M ")
      (setq display-time-interval 60)
      (setq display-time-default-load-average nil)
      (setq display-time-mail-directory nil)
      (setq display-time-mail-function nil)
      (setq display-time-use-mail-icon nil)
      (setq display-time-mail-string nil)
      (setq display-time-mail-face nil)
      (setq display-time-string-forms
            '((propertize
               (format-time-string display-time-format now)
               'face 'display-time-date-and-time
               'help-echo (format-time-string "%a %b %e, %Y" now))
              " ")))

    ;; Start Keycast at startup.  The old Home Manager config did this from
    ;; postlude.el with `(keycast-header-line-mode)', so keep the same visible
    ;; header-line behaviour in the nima setup.
    (use-package keycast
      :after prot-modeline
      ;; `:commands' made this deferred in the nima-generated config, so none
      ;; of the setup ran until a keycast mode was invoked manually.
      :demand t
      :commands (keycast-mode-line-mode
                 keycast-header-line-mode
                 keycast-tab-bar-mode
                 keycast-log-mode)
      :init
      (setq keycast-mode-line-format "%2s%k%c%R")
      (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
      (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
      (setq keycast-mode-line-remove-tail-elements nil)
      :config
      (dolist (input '(self-insert-command org-self-insert-command))
        (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
      (dolist (event '(mouse-event-p
                       mouse-movement-p
                       mwheel-scroll handle-select-window
                       mouse-set-point mouse-drag-region))
        (add-to-list 'keycast-substitute-alist `(,event nil)))
      (keycast-header-line-mode 1))
  '';
}
