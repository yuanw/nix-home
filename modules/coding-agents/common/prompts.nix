{ pkgs }:
let
  # Generate a journal-session prompt template for a specific agent
  # Returns a derivation that can be linked as ~/.pi/agent/prompts/journal-session.md
  mkJournalSessionPrompt =
    agentName:
    pkgs.writeText "journal-session.md" ''
      ---
      description: Save current ${agentName} session as a journal entry using denote-journal
      ---
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

in
{
  inherit mkJournalSessionPrompt;
}
