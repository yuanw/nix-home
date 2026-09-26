{
  config,
  pkgs,
  ...
}:
let
  ledgerDir = "${config.my.homeDirectory}/org/ledger";
  ledgerFile = "${ledgerDir}/journal.journal";
in
{
  home-manager.users.${config.my.username} = {
    home.packages = with pkgs; [
      hledger
      hledger-ui
      hledger-web
    ];

    programs.zsh.shellAliases = {
      "hls" = "hledger -f ${ledgerFile} summary";
      "hreg" = "hledger -f ${ledgerFile} register";
      "hbal" = "hledger -f ${ledgerFile} balance";
      "hw" = "hledger-web -f ${ledgerFile} -p 20299";
    };
  };
}
