_: super: {

  # To make use of this derivation, use
  # `programs.zsh.promptInit = "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";`

  zsh-powerlevel10k = super.zsh-powerlevel10k.overrideAttrs (_: {
    version = "1.13.1";
    src = super.fetchFromGitHub {
      owner = "romkatv";
      repo = "powerlevel10k";
      rev = "aa6d40b733280f154d9a55383f5efb6cf01a7a2a";
      sha256 = "0pw9zw2b4wyamsjd0zc3cah7c5m5nsb3kds4k75cl3sy6k5yqm0p";
    };
  });
}
