{
  epkgs = epkgs: [
    epkgs.dired-preview
  ];

  # https://protesilaos.com/codelog/2026-09-18-emacs-dired-preview-0-7-0/
  # 0.7.0: `dired-preview-ignored-extensions' accepts a list of extensions
  # (old regexp form still works), and `dired-preview-hook' runs after a
  # preview is shown, with `dired-preview-with-window' to act in that window.
  elisp = ''
    (use-package dired-preview
      :after dired
      :config
      ;; Upstream already ignores media, archives, and pdf/epub; this is that
      ;; default plus a few files that are never worth a preview.
      (setq dired-preview-ignored-extensions
            '("mkv" "webm" "mp4" "mp3" "ogg" "m4a" "flac" "wav"
              "gz" "zst" "tar" "xz" "rar" "zip" "iso"
              "epub" "pdf" ".DS_Store" "sqlite" "sqlite3"))
      ;; The 0.7s upstream default feels laggy when moving through a listing.
      (setq dired-preview-delay 0.2)
      (dired-preview-global-mode 1))
  '';
}
