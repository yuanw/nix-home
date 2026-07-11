{
  modalEditing,
}:

{
  my-meow = {
    enable = modalEditing == "meow";
    demand = true;
    package =
      epkgs:
      epkgs.trivialBuild {
        pname = "my-meow";
        version = "0.0.1";
        src = ../packages/my-meow.el;
        packageRequires = with epkgs; [
          meow
          avy
          embark
        ];
      };
    config = ../configs/meow.el;
  };

  hel = {
    enable = modalEditing == "hel";
    demand = true;
    config = ''
      ;; Prepend unbind handlers (t = prepend) so they run BEFORE hel's own
      ;; with-eval-after-load handlers, avoiding "key sequence z j starts
      ;; with non-prefix key z" errors in hel-integration.el.
      (eval-after-load 'compile
        '(progn
           (keymap-set compilation-mode-map "z" nil)
           (keymap-set compilation-minor-mode-map "z" nil))
        t)
      (eval-after-load 'xref
        '(keymap-set xref--xref-buffer-mode-map "z" nil)
        t)

      (require 'hel)
      (hel-mode 1)
    '';
  };

  repeat-fu = {
    enable = modalEditing == "meow";
    command = [
      "repeat-fu-mode"
      "repeat-fu-execute"
    ];
    custom = ''
      (repeat-fu-preset 'meow)
    '';
    hook = [
      ''
        (meow-mode)
         .
         (lambda ()
           (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
             (repeat-fu-mode)
             (define-key meow-normal-state-keymap (kbd "'") 'repeat-fu-execute)
             (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute)))
      ''
    ];
  };

  # Extend prot-modeline with the meow state indicator.
  # Loaded after prot-modeline so it can modify the mode-line-format.
  # noRequire prevents use-package from trying to install a package named
  # prot-modeline-meow — it's just a config fragment.
  prot-modeline-meow = {
    enable = modalEditing == "meow";
    noRequire = true;
    after = [ "prot-modeline" ];
    config = ../configs/prot-modeline-meow.el;
  };
}
