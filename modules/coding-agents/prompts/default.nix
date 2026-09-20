{
  lib,
  pkgs,
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

  claudeSkills = pkgs.claude-plugins;
  emacsSkillsDir = claudeSkills.emacs-skills;
  ponytailSkills = pkgs.pi-extensions.pi-ponytail.passthru.skills;

  emacsSkill =
    {
      name,
      description,
      extraFrontmatter ? { },
      elisp ? null,
    }:
    {
      type = "skill";
      inherit name description extraFrontmatter;
      body = skillBody "${emacsSkillsDir}/${name}/SKILL.md";
    }
    // lib.optionalAttrs (elisp != null) {
      files."${elisp}".text = builtins.readFile "${emacsSkillsDir}/${name}/${elisp}";
    };

  ponytailSkill = name: description: {
    type = "skill";
    inherit name description;
    extraFrontmatter = {
      license = "MIT";
    };
    body = skillBody "${ponytailSkills.${name}}/SKILL.md";
  };

  orgJournalTodoBody = ''
    # Add TODO items to org journal files

    Add a TODO item to the user's `~/org/journal/` directory using `emacsclient`.

    Use this when the user asks to add a todo, task, reminder, action item, or invokes `/org-journal-todo`.

    ## Filing rule

    - If the user gives no category argument, file the TODO in `~/org/journal/inbox.org`.
    - If the user gives a category argument, normalize it to a safe lowercase filename and file the TODO in `~/org/journal/<category>.org`.
    - Examples: `work` -> `~/org/journal/work.org`; `home errands` -> `~/org/journal/home-errands.org`.

    ## TODO text

    Extract the TODO text from the user's request or recent context. Keep it short, concrete, and actionable. Do not include the category in the TODO text unless it is part of the task.

    ## Command template

    Use `emacsclient --eval` so Emacs owns the file update. This preserves Org buffers correctly if they are already open.

    ```sh
    category="" # empty means inbox; otherwise use the user-provided category argument
    todo="Actionable TODO text"

    emacsclient --eval "
    (let* ((raw-category \"$category\")
           (todo \"$todo\")
           (slug (if (string-empty-p raw-category)
                     \"inbox\"
                   (replace-regexp-in-string
                    \"-+\" \"-\"
                    (replace-regexp-in-string
                     \"[^[:alnum:]]+\" \"-\"
                     (downcase (string-trim raw-category))))))
           (dir (expand-file-name \"~/org/journal/\"))
           (file (expand-file-name (concat slug \".org\") dir)))
      (require 'org)
      (make-directory dir t)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (unless (bolp) (insert \"\\n\"))
        (insert \"* TODO \" todo \"\\n  Added: \" (format-time-string \"[%Y-%m-%d %a %H:%M]\") \"\\n\")
        (save-buffer))
      file)"
    ```

    ## Safe quoting

    If the TODO text contains quotes, newlines, or shell-sensitive characters, write it to a temporary file and read it from Elisp instead of interpolating it directly.

    ```sh
    todo_file=$(mktemp /tmp/org-journal-todo.XXXXXX)
    cat > "$todo_file" <<'EOF'
    Actionable TODO text
    EOF

    emacsclient --eval "
    (let* ((raw-category \"work\")
           (todo-file \"$todo_file\")
           (todo (string-trim
                  (with-temp-buffer
                    (insert-file-contents todo-file)
                    (buffer-string))))
           (slug (if (string-empty-p raw-category)
                     \"inbox\"
                   (replace-regexp-in-string
                    \"-+\" \"-\"
                    (replace-regexp-in-string
                     \"[^[:alnum:]]+\" \"-\"
                     (downcase (string-trim raw-category))))))
           (dir (expand-file-name \"~/org/journal/\"))
           (file (expand-file-name (concat slug \".org\") dir)))
      (require 'org)
      (make-directory dir t)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (unless (bolp) (insert \"\\n\"))
        (insert \"* TODO \" todo \"\\n  Added: \" (format-time-string \"[%Y-%m-%d %a %H:%M]\") \"\\n\")
        (save-buffer))
      file)"
    ```

    ## Rules

    - Never edit the journal files with shell redirection while Emacs may have them open; use `emacsclient`.
    - Always create `~/org/journal/` if it does not exist.
    - Always report the file path that received the TODO.
    - If `emacsclient` fails, tell the user to start the Emacs server with `M-x server-start`.
  '';

  denoteNoteBody = ''
    # Create Denote notes from the current context

    Create a Denote note in Emacs using `emacsclient`. Denote creates plain text notes with predictable names like `20240322T131856--some-title__topic1_topic2.org`; the `denote` command returns the created path when called from Lisp.

    Use this when the user asks to save something as a Denote note, create a note, add a note to their knowledge base, or invokes `/denote-note`.

    ## How to create a note

    1. Extract a concise title from the user's request or recent context.
    2. Pick 1-5 lowercase keywords. Use simple words. Do not include spaces inside a keyword.
    3. Decide the note body. Preserve the useful content, not the whole chat transcript unless the user asks for a transcript.
    4. Run `emacsclient --eval` with a noninteractive Elisp form that requires `denote`, creates the note, inserts the body after Denote front matter, saves it, and prints the created path.
    5. Report the path to the user.

    ## Command template

    Write the title, keywords, and body into shell variables with safe quoting, then call Emacs:

    ```sh
    title='Short descriptive title'
    keywords='keyword1 keyword2'
    body='* Summary

    Your note body here.
    '

    emacsclient --eval "
    (let* ((title \"$title\")
           (keywords (split-string \"$keywords\" nil t))
           (body \"$body\")
           (path (progn
                   (require 'denote)
                   (denote title keywords 'org))))
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-max))
        (unless (bolp) (insert \"\\n\"))
        (insert \"\\n\" body)
        (save-buffer))
      path)"
    ```

    If the body contains quotes, newlines, or other shell-sensitive text, avoid interpolating it directly. Instead write the body to a temporary file and read it from Elisp:

    ```sh
    body_file=$(mktemp /tmp/denote-note-body.XXXXXX)
    cat > "$body_file" <<'EOF'
    * Summary

    Your note body here.
    EOF

    emacsclient --eval "
    (let* ((title \"Short descriptive title\")
           (keywords '(\"keyword1\" \"keyword2\"))
           (body-file \"$body_file\")
           (body (with-temp-buffer
                   (insert-file-contents body-file)
                   (buffer-string)))
           (path (progn
                   (require 'denote)
                   (denote title keywords 'org))))
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-max))
        (unless (bolp) (insert \"\\n\"))
        (insert \"\\n\" body)
        (save-buffer))
      path)"
    ```

    ## Denote facts to respect

    - `denote-directory` controls where notes are created; let the user's Emacs config decide it.
    - `denote` prompts interactively, but from Lisp it accepts title, keywords, and file type arguments.
    - Use `'org` unless the user asks for another file type.
    - Denote creates front matter. Insert note content after it, never before it.
    - Do not invent a custom filename. Let Denote create the identifier, title slug, keyword suffix, and extension.
    - If `emacsclient` fails, tell the user Emacs server or Denote may not be available.
  '';

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

  {
    type = "skill";
    name = "humanizer";
    description = "Remove signs of AI-generated writing from text. Use when editing or reviewing text to make it sound more natural and human-written.";
    extraFrontmatter = {
      version = "2.1.1";
    };
    body = skillBody "${claudeSkills.humanizer}/SKILL.md";
  }

  {
    type = "skill";
    name = "caveman";
    description = "Compress and simplify prompts to preserve meaning while reducing use of context.";
    body = skillBody "${claudeSkills.caveman}/SKILL.md";
  }

  {
    type = "skill";
    name = "i-have-adhd";
    description = "Shape output for a reader with ADHD: lead with the next action, number multi-step work, restate state across turns, suppress tangents, give specific time estimates, and make wins visible.";
    extraFrontmatter = {
      "disable-model-invocation" = true;
      license = "MIT";
    };
    body = skillBody "${claudeSkills.i-have-adhd}/SKILL.md";
  }

  {
    type = "skill";
    name = "org-journal-todo";
    description = "Add a TODO item to ~/org/journal/inbox.org or ~/org/journal/<category>.org using emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
      "argument-hint" = "[category] TODO text";
    };
    body = orgJournalTodoBody;
  }

  {
    type = "skill";
    name = "denote-note";
    description = "Create a Denote note in Emacs from the current context using emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    body = denoteNoteBody;
  }

  (emacsSkill {
    name = "d2";
    description = "Create a diagram from the current context using D2 and output the resulting image path.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
  })

  (emacsSkill {
    name = "describe";
    description = "Look up Emacs documentation via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    elisp = "agent-skill-describe.el";
  })

  (emacsSkill {
    name = "dired";
    description = "Open files from the latest interaction in an Emacs dired buffer via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    elisp = "agent-skill-dired.el";
  })

  (emacsSkill {
    name = "emacsclient";
    description = "Always use emacsclient instead of emacs. This applies to all Emacs operations: user requests, byte compilation, check-parens, running ERT tests, and any other elisp evaluation.";
    extraFrontmatter = {
      tools = "Bash";
    };
  })

  (emacsSkill {
    name = "file-links";
    description = "When referencing files, format them as markdown links with line numbers using GitHub-style #L syntax.";
  })

  (emacsSkill {
    name = "gnuplot";
    description = "Plot data from the current context using gnuplot and output the resulting image path.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
  })

  (emacsSkill {
    name = "highlight";
    description = "Highlight relevant regions in one or more files in Emacs via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    elisp = "agent-skill-highlight.el";
  })

  (emacsSkill {
    name = "mermaid";
    description = "Create a diagram from the current context using Mermaid and output the resulting image path.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
  })

  (emacsSkill {
    name = "open";
    description = "Open files from the latest interaction in Emacs buffers via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    elisp = "agent-skill-open.el";
  })

  (emacsSkill {
    name = "plantuml";
    description = "Create a diagram from the current context using PlantUML and output the resulting image path.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
  })

  (emacsSkill {
    name = "select";
    description = "Open one or more files in Emacs and select a region relevant to the current discussion via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
    };
    elisp = "agent-skill-select.el";
  })

  (ponytailSkill "ponytail" "Lazy senior dev mode: force the simplest solution that actually works, prefer stdlib/native features, and stop before over-engineering.")
  (ponytailSkill "ponytail-audit" "Whole-repo audit for over-engineering: a ranked list of what to delete, simplify, or replace with stdlib/native equivalents.")
  (ponytailSkill "ponytail-debt" "Harvest every ponytail deferral comment into a debt ledger. One-shot report, changes nothing.")
  (ponytailSkill "ponytail-gain" "Show ponytail's measured impact as a compact scoreboard. One-shot display, not a persistent mode.")
  (ponytailSkill "ponytail-help" "Quick-reference card for all ponytail modes, skills, and commands.")
  (ponytailSkill "ponytail-review" "Review a diff for over-engineering only: what to delete, simplify, or replace with stdlib/native equivalents.")
]
