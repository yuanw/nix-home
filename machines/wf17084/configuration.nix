{ config, lib, ... }: {
  imports = lib.attrValues (import ../../modules);
  networking.hostName = "wf17084";

  programs = {
    dart.enable = true;
    workShell.enable = true;
    editors.emacs.enable = true;
  };

  home-manager.users.yuanwang.programs.git = {
    userEmail = "yuan.wang@workiva.com";

    signing = {
      key = "19AD3F6B1A5BF3BF";
      signByDefault = true;
    };

    extraConfig = { github.user = "yuanwang-wf"; };
  };
}
