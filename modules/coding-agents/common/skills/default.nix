{ pkgs }:
let
  # Build a skill package from inline markdown (Pi / Claude Code / Cursor conventions).
  # The store path is the skill directory (contains SKILL.md). `pname` becomes the
  # directory name under ~/.pi/agent/skills/<pname>/ etc.
  mkSkill =
    {
      pname,
      text,
      version ? "0",
      rev ? "local",
    }:
    pkgs.runCommand "${pname}-skill"
      {
        inherit pname version;
        passthru.claudeSkill = {
          inherit pname version rev;
        };
      }
      ''
        mkdir -p $out
        cp ${pkgs.writeText "SKILL.md" text} $out/SKILL.md
      '';

  # Package a repo-local SKILL.md (path must live under this flake so it is copied to the store).
  mkSkillFromPath =
    {
      pname,
      src,
      version ? "0",
      rev ? "local",
    }:
    pkgs.runCommand "${pname}-skill"
      {
        inherit pname version;
        passthru.claudeSkill = {
          inherit pname version rev;
        };
      }
      ''
        mkdir -p $out
        cp ${src} $out/SKILL.md
      '';

  # Package a repo-local directory (e.g. SKILL.md plus referenced *.md assets).
  mkSkillFromDir =
    {
      pname,
      src,
      version ? "0",
      rev ? "local",
    }:
    pkgs.runCommand "${pname}-skill"
      {
        inherit pname version;
        passthru.claudeSkill = {
          inherit pname version rev;
        };
      }
      ''
        mkdir -p $out
        cp -r ${src}/. $out/
      '';

  mkJournalSessionSkill =
    agentName:
    mkSkill {
      pname = "journal-session";
      text = ''
        Save the current ${agentName} session as a journal entry using emacsclient and denote-journal.

        Steps:
        1. Summarize the current conversation: list the key topics discussed, decisions made, commands run, and any important outputs or conclusions. Format as an org-mode outline.

        2. Compose an elisp expression to append the session to today's denote journal:
           - Call `(denote-journal-new-or-existing-entry)` to open or create today's journal file
           - Move to end of buffer with `(goto-char (point-max))`
           - Insert a top-level org heading with timestamp: `* ${agentName} Session - HH:MM\n`
           - Insert the formatted summary as org content
           - Save the buffer with `(save-buffer)`

        3. Run the elisp via emacsclient:
           ```
           emacsclient --eval "(progn
             (require 'denote-journal)
             (denote-journal-new-or-existing-entry)
             (with-current-buffer (current-buffer)
               (goto-char (point-max))
               (insert \"\\n* ${agentName} Session - \" (format-time-string \"%H:%M\") \"\\n\")
               (insert \"SUMMARY_CONTENT\")
               (save-buffer)))"
           ```
           Replace SUMMARY_CONTENT with the actual org-formatted summary from step 1.
           Escape all double quotes in the content with backslash.

        4. If emacsclient fails (Emacs server not running), inform the user and suggest they start the Emacs server with `M-x server-start` or add `(server-start)` to their Emacs config.

        5. On success, report the denote journal file path (located in ~/org/denote/journal/) where the session was saved.

        Notes:
        - Journal files are stored in ~/org/denote/journal/
        - The denote file for today will have the format: YYYYMMDDTHHMMSS--TITLE__journal.org
        - The C-c n t keybinding also opens today's journal interactively in Emacs
      '';
    };

in
{
  inherit
    mkSkill
    mkSkillFromPath
    mkSkillFromDir
    mkJournalSessionSkill
    ;

  # Skill derivations only (use like `pkgs.codingAgentsSkillPackages.grilling`).
  packages = {
    grilling = mkSkillFromPath {
      pname = "grilling";
      src = ./grilling/SKILL.md;
    };
    teach = mkSkillFromDir {
      pname = "teach";
      src = ./teach;
    };
  };
}
