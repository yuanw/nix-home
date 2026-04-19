{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let

  cfg = config.programs.emacs.init;
  packageFunctionType = mkOptionType {
    name = "packageFunction";
    description = "function from epkgs to package";
    check = isFunction;
    merge = mergeOneOption;
  };

  usePackageType = types.submodule (
    { name, config, ... }:
    {
      options = {
        enable = mkEnableOption "Emacs package ${name}";

        package = mkOption {
          type = types.either (types.str // { description = "name of package"; }) packageFunctionType;
          default = name;
          description = ''
            The package to use for this module. Either the package name
            within the Emacs package set or a function taking the Emacs
            package set and returning a package.
          '';
        };

        noRequire = mkOption {
          type = types.bool;
          default = false;
          description = ''
            The <option>:no-require</option> setting.
          '';
        };

        defer = mkOption {
          type = types.either types.bool types.ints.positive;
          default = false;
          description = ''
            The <option>:defer</option> setting.
          '';
        };

        defines = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:defines</option>.
          '';
        };

        demand = mkOption {
          type = types.bool;
          default = false;
          description = ''
            The <option>:demand</option> setting.
          '';
        };

        diminish = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:diminish</option>.
          '';
        };

        chords = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "jj" = "ace-jump-char-mode";
            "jk" = "ace-jump-word-mode";
          };
          description = ''
            The entries to use for <option>:chords</option>.
          '';
        };

        functions = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:functions</option>.
          '';
        };

        mode = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:mode</option>.
          '';
        };

        after = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''

            The entries to use for <option>:after</option>.
          '';
        };

        bind = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "M-<up>" = "drag-stuff-up";
            "M-<down>" = "drag-stuff-down";
          };
          description = ''
            The entries to use for <option>:bind</option>.
          '';
        };

        bindLocal = mkOption {
          type = types.attrsOf (types.attrsOf types.str);
          default = { };
          example = {
            helm-command-map = {
              "C-c h" = "helm-execute-persistent-action";
            };
          };
          description = ''
            The entries to use for local keymaps in <option>:bind</option>.
          '';
        };

        bindKeyMap = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "C-c p" = "projectile-command-map";
          };
          description = ''
            The entries to use for <option>:bind-keymap</option>.
          '';
        };

        custom = mkOption {
          type = types.lines;
          default = "";
          description = ''
            The entries to use for <option>:custom</option>.
          '';

        };
        customFace = mkOption {
          type = types.attrsOf types.str;
          default = { };
          example = {
            "org-habit-alert-face" = "(((background light)) (:background \"#f5f946\"))";
          };
          description = ''
            The entries to use for <option>:custom-face</option>.
          '';
        };

        command = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:commands</option>.
          '';
        };

        preface = mkOption {
          type = types.either types.lines types.path;
          default = "";
          description = ''
            Code to place in the <option>:preface</option> section.
            When a path is given, the file is loaded at runtime instead of
            being embedded, enabling hot-reload.
          '';
        };

        config = mkOption {
          type = types.either types.lines types.path;
          default = "";
          description = ''
            Code to place in the <option>:config</option> section.
          '';
        };

        extraConfig = mkOption {
          type = types.either types.lines types.path;
          default = "";
          description = ''
            Additional lines to place in the use-package configuration.
            When a path is given, the file is loaded at runtime instead of
            being embedded, enabling hot-reload.
          '';
        };

        hook = mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            The entries to use for <option>:hook</option>.
          '';
        };

        earlyInit = mkOption {
          type = types.lines;
          default = "";
          description = ''
            Lines to add to <option>programs.emacs.init.earlyInit</option> when
            this package is enabled.
            </para><para>
            Note, the package is not automatically loaded so you will have to
            <literal>require</literal> the necessary features yourself.
          '';
        };

        init = mkOption {
          type = types.either types.lines types.path;
          default = "";
          description = ''
            The entries to use for <option>:init</option>.
            When a path is given, the file is loaded at runtime instead of
            being embedded, enabling hot-reload.
          '';
        };

        extraPackages = mkOption {
          type = types.listOf types.package;
          default = [ ];
          description = ''
            Extra packages to add to <option>home.packages</option>.
          '';
        };

        assembly = mkOption {
          type = types.lines;
          readOnly = true;
          internal = true;
          description = "The final use-package code.";
        };
      };

      config = mkIf config.enable {
        assembly =
          let
            quoted = v: ''"${escape [ ''"'' ] v}"'';
            mkBindHelper =
              cmd: prefix: bs:
              optionals (bs != { }) (
                [ ":${cmd} (${prefix}" ] ++ mapAttrsToList (n: v: "  (${quoted n} . ${v})") bs ++ [ ")" ]
              );

            mkAfter = vs: optional (vs != [ ]) ":after (${toString vs})";
            mkCustom = vs: optional (vs != "") ":custom ${toString vs}";
            mkCommand = vs: optional (vs != [ ]) ":commands (${toString vs})";
            mkDefines = vs: optional (vs != [ ]) ":defines (${toString vs})";
            mkDiminish = vs: optional (vs != [ ]) ":diminish (${toString vs})";
            mkMode = map (v: ":mode ${v}");
            mkFunctions = vs: optional (vs != [ ]) ":functions (${toString vs})";
            mkBind = mkBindHelper "bind" "";
            mkBindLocal =
              bs:
              let
                mkMap = n: v: mkBindHelper "bind" ":map ${n}" v;
              in
              flatten (mapAttrsToList mkMap bs);
            mkBindKeyMap = mkBindHelper "bind-keymap" "";
            mkChords = mkBindHelper "chords" "";
            mkCustomFace = mkBindHelper "custom-face" "";
            mkHook = map (v: ":hook ${v}");
            mkNoRequire = v: optional v ":no-require t";
            mkDefer = v: if isBool v then optional v ":defer t" else [ ":defer ${toString v}" ];
            mkDemand = v: optional v ":demand t";

            # When a Nix path is given, emit (hm--load-external "...") instead of
            # embedding content.  This allows Emacs to load the file at runtime,
            # enabling hot-reload when an external version exists in
            # `user-emacs-directory/external/'.
            mkElispOrLoad =
              content: if builtins.isPath content then "(hm--load-external \"${toString content}\")" else content;
          in
          concatStringsSep "\n  " (
            [ "(use-package ${name}" ]
            ++ mkNoRequire config.noRequire
            ++ mkAfter config.after
            ++ mkBind config.bind
            ++ mkBindKeyMap config.bindKeyMap
            ++ mkBindLocal config.bindLocal
            ++ mkChords config.chords
            ++ mkCustom config.custom
            ++ mkCustomFace config.customFace
            ++ mkCommand config.command
            ++ mkDefer config.defer
            ++ mkDefines config.defines
            ++ mkFunctions config.functions
            ++ mkDemand config.demand
            ++ mkDiminish config.diminish
            ++ mkHook config.hook
            ++ mkMode config.mode
            ++ optionals (config.init != "") [
              ":init"
              (mkElispOrLoad config.init)
            ]
            ++ optionals (config.config != "" && config.config != null) [
              ":config"
              (mkElispOrLoad config.config)
            ]
            ++ optionals (config.preface != "") [
              ":preface"
              (mkElispOrLoad config.preface)
            ]
            ++ optional (config.extraConfig != "") (mkElispOrLoad config.extraConfig)
          )
          + ")";
      };
    }
  );

  mkRecommendedOption =
    type: extraDescription:
    mkOption {
      type = types.bool;
      default = false;
      example = true;
      description = ''
        Whether to enable recommended ${type} settings.
      ''
      + optionalString (extraDescription != "") ''
        </para><para>
        ${extraDescription}
      '';
    };

  # Recommended GC settings.
  gcSettings = ''
    (defun hm/reduce-gc ()
      "Reduce the frequency of garbage collection."
      (setq gc-cons-threshold most-positive-fixnum
            gc-cons-percentage 0.6))

    (defun hm/restore-gc ()
      "Restore the frequency of garbage collection."
      (setq gc-cons-threshold 16777216
            gc-cons-percentage 0.1))

    ;; Make GC more rare during init, while minibuffer is active, and
    ;; when shutting down. In the latter two cases we try doing the
    ;; reduction early in the hook.
    (hm/reduce-gc)
    (add-hook 'minibuffer-setup-hook #'hm/reduce-gc -50)
    (add-hook 'kill-emacs-hook #'hm/reduce-gc -50)

    ;; But make it more regular after startup and after closing minibuffer.
    (add-hook 'emacs-startup-hook #'hm/restore-gc)
    (add-hook 'minibuffer-exit-hook #'hm/restore-gc)

    ;; Avoid unnecessary regexp matching while loading .el files.
    (defvar hm/file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)

    (defun hm/restore-file-name-handler-alist ()
      "Restores the file-name-handler-alist variable."
      (setq file-name-handler-alist hm/file-name-handler-alist)
      (makunbound 'hm/file-name-handler-alist))

    (add-hook 'emacs-startup-hook #'hm/restore-file-name-handler-alist)
  '';

  # Whether the configuration makes use of `:diminish`.
  hasDiminish = any (p: p.diminish != [ ]) (attrValues cfg.usePackage);

  # Whether the configuration makes use of `:bind`.
  hasBind = any (p: p.bind != { } || p.bindLocal != { } || p.bindKeyMap != { }) (
    attrValues cfg.usePackage
  );

  # Whether the configuration makes use of `:chords`.
  hasChords = any (p: p.chords != { }) (attrValues cfg.usePackage);

  usePackageSetup = ''
    (eval-when-compile
      (require 'use-package)
      ;; To help fixing issues during startup.
      (setq use-package-verbose ${if cfg.usePackageVerbose then "t" else "nil"}))

  ''
  + optionalString hasDiminish ''
    ;; For :diminish in (use-package).
    (require 'diminish)
  ''
  + optionalString hasBind ''
    ;; For :bind in (use-package).
    (require 'bind-key)

    ;; Fixes "Symbol’s function definition is void: use-package-autoload-keymap".
    (autoload #'use-package-autoload-keymap "use-package-bind-key")
  ''
  + optionalString hasChords ''
    ;; For :chords in (use-package).
    (use-package use-package-chords
      :config (key-chord-mode 1))
  '';

  earlyInitFile = ''
    ;;; hm-early-init.el --- Emacs configuration à la Home Manager -*- lexical-binding: t; -*-
    ;;
    ;;; Commentary:
    ;;
    ;; The early init component of the Home Manager Emacs configuration.
    ;;
    ;;; Code:

    ${cfg.earlyInit}

    (provide 'hm-early-init)
    ;; hm-early-init.el ends here
  '';

  initFile = ''
        ;;; hm-init.el --- Emacs configuration à la Home Manager -*- lexical-binding: t; -*-
        ;;
        ;;; Commentary:
        ;;
        ;; A configuration generated from a Nix based configuration by
        ;; Home Manager.
        ;;
        ;;; Code:

        ${optionalString cfg.startupTimer ''
          (defun hm/print-startup-stats ()
            "Prints some basic startup statistics."
            (let ((elapsed (float-time (time-subtract after-init-time
                                                      before-init-time))))
              (message "Startup took %.2fs with %d GCs" elapsed gcs-done)))
          (add-hook 'emacs-startup-hook #'hm/print-startup-stats)
        ''}

        ;; ── Hot-reload infrastructure ──────────────────────────────────────
        (defvar hm-external-config-files nil
          "List of external config files loaded via hm-init.
    Each entry is an absolute file path.  When `hm-hot-reload-mode' is
    active, these files are watched for changes and auto-reloaded.")

        (defun hm--load-external (store-path)
          "Load a config file, preferring external version over store version.
    If the file basename exists in `user-emacs-directory/external/', use
    that (hot-reloadable).  Otherwise fall back to STORE-PATH (immutable)."
          (let* ((basename (file-name-nondirectory store-path))
                 (ext-dir (expand-file-name "external" user-emacs-directory))
                 (ext-path (expand-file-name basename ext-dir))
                 (effective-path (if (and (file-directory-p ext-dir)
                                          (file-exists-p ext-path))
                                     ext-path
                                   store-path)))
            (add-to-list 'hm-external-config-files effective-path)
            (load effective-path t)))

        (defun hm/reload-config ()
          "Reload all external config files."
          (interactive)
          (dolist (file hm-external-config-files)
            (when (file-exists-p file)
              (load file t)))
          (message "Reloaded %d config files" (length hm-external-config-files)))

        (defvar hm--file-watchers nil
          "File-notify watch descriptors for hot-reload.")

        (defun hm--watch-config-file (file)
          "Watch FILE for changes and reload it."
          (when (and (file-exists-p file) (fboundp 'file-notify-add-watch))
            (push (file-notify-add-watch
                    file '(change)
                    (lambda (_event)
                      (when (file-exists-p file)
                        (load file t)
                        (message "Hot-reloaded: %s" file))))
                  hm--file-watchers)))

        (defun hm--unwatch-config-files ()
          "Stop watching all config files."
          (dolist (w hm--file-watchers)
            (file-notify-rm-watch w))
          (setq hm--file-watchers nil))

        (define-minor-mode hm-hot-reload-mode
          "Auto-reload external config files on change."
          :global t
          :lighter " \U0001F525"
          (if hm-hot-reload-mode
              (dolist (f hm-external-config-files)
                (hm--watch-config-file f))
            (hm--unwatch-config-files)))
        ;; ── End hot-reload infrastructure──────────────────────────────────

        ${cfg.prelude}

        ${usePackageSetup}
  ''
  + concatStringsSep "\n\n" (
    map (getAttr "assembly") (filter (getAttr "enable") (attrValues cfg.usePackage))
  )
  + ''

    ${cfg.postlude}

    ;; Load personal config if it exists — no rebuild needed
    (let ((personal-file (expand-file-name "personal.el" user-emacs-directory)))
      (when (file-exists-p personal-file)
        (load personal-file t)))

    (provide 'hm-init)
    ;; hm-init.el ends here
  '';

in
{
  imports = [ ./emacs-init-defaults.nix ];

  options.programs.emacs.init = {
    enable = mkEnableOption "Emacs configuration";

    recommendedGcSettings = mkRecommendedOption "garbage collection" ''
      This will reduce garbage collection frequency during startup and
      while the minibuffer is active.
    '';

    startupTimer = mkEnableOption "Emacs startup duration timer";

    earlyInit = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in <filename>early-init.el</filename>.
      '';
    };

    prelude = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in the beginning of
        <filename>init.el</filename>.
      '';
    };

    postlude = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Configuration lines to add in the end of
        <filename>init.el</filename>.
      '';
    };

    packageQuickstart = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to enable package-quickstart. This will make sure that
        <literal>package.el</literal> is activated and all autoloads are
        available.
        </para><para>
        If disabled you can save quite a few milliseconds on the startup time,
        but you will most likely have to tweak the <literal>command</literal>
        option of various packages.
        </para><para>
        As an example, running <literal>(emacs-init-time)</literal> on an Emacs
        configuration with this option enabled reported ~300ms. Disabling the
        option dropped the init time to ~200ms.
      '';
    };

    usePackageVerbose = mkEnableOption "verbose use-package mode";

    usePackage = mkOption {
      type = types.attrsOf usePackageType;
      default = { };
      example = literalExpression ''
        {
          dhall-mode = {
            mode = [ '''"\\.dhall\\'"''' ];
          };
        }
      '';
      description = ''
        Attribute set of use-package configurations.
      '';
    };
  };

  config = mkIf (config.programs.emacs.enable && cfg.enable) {
    # Collect the extra packages that should be included in the user profile.
    # These are typically tools called by Emacs packages.
    home.packages = concatMap (v: v.extraPackages) (
      filter (getAttr "enable") (builtins.attrValues cfg.usePackage)
    );

    programs.emacs.init.earlyInit =
      let

        standardEarlyInit = mkBefore ''
          ${optionalString cfg.recommendedGcSettings gcSettings}

          ${
            if cfg.packageQuickstart then
              ''
                (setq package-quickstart t
                      package-quickstart-file "hm-package-quickstart.el")
              ''
            else
              ''
                (setq package-enable-at-startup nil)
              ''
          }

          ;; Avoid expensive frame resizing. Inspired by Doom Emacs.
          (setq frame-inhibit-implied-resize t)
        '';

        # Collect the early initialization strings for each package.
        packageEarlyInits = map (p: p.earlyInit) (
          filter (p: p.earlyInit != "") (builtins.attrValues cfg.usePackage)
        );

      in
      mkMerge ([ standardEarlyInit ] ++ packageEarlyInits);

    programs.emacs.extraPackages =
      epkgs:
      let
        getPkg =
          v: if isFunction v then [ (v epkgs) ] else optional (isString v && hasAttr v epkgs) epkgs.${v};

        packages = concatMap (v: getPkg (v.package)) (
          filter (s: !(getAttr "noRequire" s)) (
            filter (getAttr "enable") (builtins.attrValues cfg.usePackage)
          )
        );
      in
      [
        (epkgs.trivialBuild {
          pname = "hm-early-init";
          src = pkgs.writeText "hm-early-init.el" earlyInitFile;
          version = "0.1.2";
          packageRequires = packages;
          preferLocalBuild = true;
          allowSubstitutes = false;
        })

        (epkgs.trivialBuild {
          pname = "hm-init";
          src = pkgs.writeText "hm-init.el" initFile;
          version = "0.1.2";
          packageRequires = [
            epkgs.use-package
          ]
          ++ packages
          ++ optional hasBind epkgs.bind-key
          ++ optional hasDiminish epkgs.diminish
          ++ optional hasChords epkgs.use-package-chords;
          preferLocalBuild = true;
          allowSubstitutes = false;
          preBuild = ''
            # Do a bit of basic formatting of the generated init file.
            emacs -Q --batch \
              --eval '(find-file "hm-init.el")' \
              --eval '(let ((indent-tabs-mode nil) (lisp-indent-offset 2)) (indent-region (point-min) (point-max)))' \
              --eval '(write-file "hm-init.el")'

            ${optionalString cfg.packageQuickstart ''
              # Generate a package quickstart file to make autoloads and such
              # available.
              emacs -Q --batch \
                --eval "(require 'package)" \
                --eval "(setq package-quickstart-file \"hm-package-quickstart.el\")" \
                --eval "(package-quickstart-refresh)"

              # We know what we're doing?
              sed -i '/no-byte-compile: t/d' hm-package-quickstart.el
            ''}
          '';
        })
      ];

    home.file = {
      ".emacs.d/early-init.el".text = ''
        ;; -*- lexical-binding: t -*-
        (require 'hm-early-init)
        (provide 'early-init)
      '';

      ".emacs.d/init.el".text = ''
        ;; -*- lexical-binding: t -*-
        (require 'hm-init)
        (provide 'init)
      '';
    };
  };
}
