;;; consult-omni.el --- consult-omni configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; consult-omni setup and app launcher integration.

;;; Code:

(defun my-consult-omni-setup ()
  "Configure consult-omni sources and app search paths."
  ;; Load Sources Core code.
  (require 'consult-omni-sources)

  (setq consult-omni-sources-modules-to-load
        '(consult-omni-apps
          consult-omni-calc
          consult-omni-consult-notes
          consult-omni-dict
          consult-omni-notes
          consult-omni-org-agenda
          consult-omni-wikipedia))
  (consult-omni-sources-load-modules)

  ;; Load Embark actions.
  (require 'consult-omni-embark nil t)

  (setq consult-omni-apps-paths
        (append (file-expand-wildcards "/Applications/Adobe*")
                '("/Applications"
                  "/Applications/Utilities/"
                  "/Applications/Nix Casks/"
                  "/System/Applications/"
                  "/System/Applications/Utilities/"
                  "/System/Library/CoreServices/Applications/"
                  "~/Applications/"
                  "~/Applications/Home Manager Apps")))

  ;;; Set multiple sources for consult-omni-multi command. Change these lists as needed
  ;;; for different interactive commands. Each source must be a key in
  ;;; `consult-omni-sources-alist'.
  (setq consult-omni-multi-sources
        '("calc"
          ;; "File"
          ;; "Buffer"
          ;; "Bookmark"
          "Apps"
          ;; "gptel"
          ;; "Brave"
          "Dictionary"
          ;; "Google"
          "Wikipedia"
          ;; "elfeed"
          ;; "mu4e"
          ;; "buffers text search"
          "Notes Search"
          "Org Agenda"
          ;; "GitHub"
          ;; "YouTube"
          ;; "Invidious"
          ))

  ;;; Set your shorthand favorite interactive command.
  (setq consult-omni-default-interactive-command #'consult-omni-multi))

(defun consult-omni-app-launcher ()
  "Open a temporary app-launcher frame and launch an app with consult-omni."
  (interactive)
  (my-consult-omni-setup)
  (let* ((vertico-count 30)
         (width (floor (* 0.8 (display-pixel-width))))
         (height (floor (* 0.8 (display-pixel-height))))
         (left (floor (* 0.1 (display-pixel-width))))
         (top (floor (* 0.1 (display-pixel-height))))
         (params `((name . "demo-omni")
                   (width . ,(cons 'text-pixels width))
                   (height . ,(cons 'text-pixels height))
                   (left . ,left)
                   ;; Only works on macOS, and is needed for the launcher frame.
                   (window-system . ns)
                   (top . ,top)
                   (minibuffer . only)))
         (frame (make-frame params)))
    (with-selected-frame frame
      (select-frame-set-input-focus (selected-frame))
      (unwind-protect
          (consult-omni-apps-static ".*" (propertize "  " 'face 'consult-omni-path-face))
        (when (frame-live-p frame)
          (delete-frame frame))))))

(use-package consult-omni
  :after consult
  :commands (consult-omni consult-omni-app-launcher)
  :config
  (my-consult-omni-setup))

(provide 'nima-feature-consult-omni)
;;; consult-omni.el ends here
