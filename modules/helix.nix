# https://github.com/maaslalani/_/blob/main/modules/helix.nix
{ config, ... }: {

  home-manager.users.${config.my.username} = {

    programs.helix = {
      enable = true;
      settings = {
        editor = {
          gutters = [ "diff" "line-numbers" "spacer" "diagnostics" ];
          cursorline = true;
          cursor-shape.insert = "bar";
          true-color = true;
          lsp.display-messages = true;
        };
        theme = "charm";
        keys.insert.esc = [ "collapse_selection" "normal_mode" ];
        keys.normal.esc = [ "collapse_selection" "normal_mode" ];
        keys.select.esc =
          [ "collapse_selection" "keep_primary_selection" "normal_mode" ];
        keys.normal = {
          X = "extend_line_above";
          a = [ "append_mode" "collapse_selection" ];
          g.q = ":reflow";
          i = [ "insert_mode" "collapse_selection" ];
          ret = [ "move_line_down" "goto_line_start" ];
          space = {
            w = ":write";
            q = ":quit";
          };
        };
      };
      languages =
        {
          language = [
            {
              name = "nix";
              auto-format = true;
              formatter = { command = "alejandra"; };
              language-server = { command = "nil"; };
            }
            {
              name = "html";
              auto-format = false;
            }
          ];

        };
    };
  };
}
