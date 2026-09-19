{
  lib,
  ...
}:
let
  stripFrontmatter =
    text:
    let
      parts = lib.splitString "\n---\n" text;
    in
    if lib.hasPrefix "---\n" text && builtins.length parts > 1 then
      lib.removePrefix "\n" (lib.concatStringsSep "\n---\n" (builtins.tail parts))
    else
      text;

  skillBody = path: stripFrontmatter (builtins.readFile path);

  journalSessionBody = agentName: ''
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
           (insert "\n* ${agentName} Session - " (format-time-string "%H:%M") "\n")
           (insert "SUMMARY_CONTENT")
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
in
[
  {
    type = "command";
    name = "journal-session";
    description = "Save current pi session as a journal entry using denote-journal.";
    body = journalSessionBody "pi";
  }

  {
    type = "skill";
    name = "journal-session";
    description = "Save the current agent session as a journal entry using emacsclient and denote-journal.";
    body = journalSessionBody "Agent";
  }

  {
    type = "skill";
    name = "disk-space";
    description = "Find local files and caches that are candidates for deletion to save disk space. Use when the user asks to free space, clean up disk, find large files, audit caches, or prune containers.";
    extraFrontmatter = {
      "allowed-tools" = "Bash, Read, Grep, Glob";
    };
    body = skillBody ../common/skills/disk-space/SKILL.md;
  }

  {
    type = "skill";
    name = "explain-diff-html";
    description = "Use when the user asks for a rich explanation of a code change, diff, branch, or PR. Produces HTML output.";
    body = skillBody ../common/skills/explain-diff-html/SKILL.md;
  }

  {
    type = "skill";
    name = "grilling";
    description = "Interview the user relentlessly about a plan or design. Use when the user wants to stress-test a plan before building, or uses any grill trigger phrases.";
    body = skillBody ../common/skills/grilling/SKILL.md;
  }

  {
    type = "skill";
    name = "teach";
    description = "Teach the user a new skill or concept, within this workspace.";
    extraFrontmatter = {
      "disable-model-invocation" = true;
      "argument-hint" = "What would you like to learn about?";
    };
    body = skillBody ../common/skills/teach/SKILL.md;
    files = {
      "GLOSSARY-FORMAT.md".text = builtins.readFile ../common/skills/teach/GLOSSARY-FORMAT.md;
      "LEARNING-RECORD-FORMAT.md".text =
        builtins.readFile ../common/skills/teach/LEARNING-RECORD-FORMAT.md;
      "MISSION-FORMAT.md".text = builtins.readFile ../common/skills/teach/MISSION-FORMAT.md;
      "RESOURCES-FORMAT.md".text = builtins.readFile ../common/skills/teach/RESOURCES-FORMAT.md;
    };
  }
]
