{ config
, pkgs
, lib
, astro-nvim
, ...
}:
let
  langs = [
    # "agda"
    "bash"
    "c"
    # "c-sharp"
    # "cpp"
    "css"
    # "elm"
    "elisp"
    #"fluent"
    "go"
    "hcl"
    "haskell"
    "html"
    # "janet-simple"
    "java"
    "javascript"
    # "jsdoc"
    "json"
    "julia"
    # "ocaml"
    # "pgn"
    # "php"
    "python"
    # "ruby"
    # "rust"
    # "scala"
    # "swift"
    # "typescript"
    "yaml"
    "nix"
    "lua"
    "markdown-inline"
    # "perl"
    "make"
    "toml"
  ];
  pyPkgs = pkgs.python3Packages;
  # nvim-open = pkgs.stdenv.mkDerivation {
  #   name = "nvim-open";
  #   unpackPhase = ":";
  #   nativeBuildInputs = [ pyPkgs.wrapPython ];
  #   pythonPath = [ (pyPkgs.toPythonModule pkgs.neovim-remote) ];
  #   preFixup = "wrapPythonPrograms";
  #   installPhase = ''
  #     install -D -m755 ${./nvim-open.py} $out/bin/nvim-open
  #   '';
  # };
in
{

    home-manager.users.${config.my.username} = {
  home.packages = with pkgs; [
    neovim
    # nvim-open

    nodejs # copilot
    vale
    terraform-ls
    nodePackages.pyright
    sumneko-lua-language-server

    # based on ./suggested-pkgs.json
    delve
    # gopls
    # golangci-lint
    nodePackages.bash-language-server
    taplo-lsp
    marksman
    rust-analyzer
    yaml-language-server
    nil
    # gomodifytags
    # gofumpt
    iferr
    impl
    # gotools
    shellcheck
    shfmt
    # isort
    black
    ruff
    nixpkgs-fmt
    terraform-ls
    clang-tools
    nodePackages.prettier
    stylua
    # does not build yet on aarch64
  ] ++ lib.optional (pkgs.stdenv.hostPlatform.system == "x86_64-linux") pkgs.deno; # lsp
  xdg.dataHome = "${config.my.homeDirectory}/.data";
  xdg.dataFile."nvim/lazy/telescope-fzf-native.nvim/build/libfzf.so".source = "${pkgs.vimPlugins.telescope-fzf-native-nvim}/build/libfzf.so";
  xdg.configFile."nvim".source = pkgs.runCommand "nvim" { } ''
    mkdir -p $out/parser

    ln -s ${astro-nvim}/* $out/
    rm $out/lua
    mkdir -p $out/lua
    ln -s ${astro-nvim}/lua/* $out/lua

    ${lib.concatMapStringsSep "\n" (name: ''
      ln -s ${pkgs.tree-sitter.builtGrammars."tree-sitter-${name}"}/parser $out/parser/${name}.so
    '') langs}
  '';

  };
}
