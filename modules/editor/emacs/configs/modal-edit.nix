{
  packagePath,
  pkgs,
  modalEditing,
}:

let
  helPackage =
    epkgs:
    pkgs.callPackage "${packagePath}/hel.nix" {
      inherit (epkgs) melpaBuild;
    };
in

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
    package = helPackage;
    config = ''
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
}
