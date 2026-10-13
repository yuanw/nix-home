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

  orgAgendaTodoBody = ''
    # Add TODO items to org agenda files

    Add a TODO item to the user's `~/org/agenda/` directory using `emacsclient`.

    Use this when the user asks to add a todo, task, reminder, action item, or invokes `/org-agenda-todo`.

    ## Filing rule

    - If the user gives no category argument, file the TODO in `~/org/agenda/inbox.org`.
    - If the user gives a category argument, normalize it to a safe lowercase filename and file the TODO in `~/org/agenda/<category>.org`.
    - Examples: `work` -> `~/org/agenda/work.org`; `home errands` -> `~/org/agenda/home-errands.org`.

    ## TODO text

    Extract the TODO text from the user's request or recent context. Keep it short, concrete, and actionable. Do not include the category in the TODO text unless it is part of the task.

    ## Command template

    Use `emacsclient --eval` so Emacs owns the file update. This preserves Org buffers correctly if they are already open.

    Write the entry block to a temp file and read it from Elisp, so quoting and
    newlines are never a problem:

    ```sh
    category="work"   # empty means inbox
    entry_file=$(mktemp /tmp/org-agenda-todo.XXXXXX)
    cat > "$entry_file" <<'EOF'
    Actionable TODO text
    EOF

    emacsclient --eval "
    (let* ((raw-category \"$category\")
           (entry-file \"$entry_file\")
           (entry (string-trim
                   (with-temp-buffer
                     (insert-file-contents entry-file)
                     (buffer-string))))
           (slug (if (string-empty-p raw-category)
                     \"inbox\"
                   (replace-regexp-in-string
                    \"-+\" \"-\"
                    (replace-regexp-in-string
                     \"[^[:alnum:]]+\" \"-\"
                     (downcase (string-trim raw-category))))))
           (dir (expand-file-name \"~/org/agenda/\"))
           (file (expand-file-name (concat slug \".org\") dir)))
      (require 'org)
      (make-directory dir t)
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-max))
        (unless (bolp) (insert \"\\n\"))
        (insert
         (if (string-prefix-p \"*\" entry t)
             (replace-regexp-in-string \"%now%\" (format-time-string \"[%Y-%m-%d %a %H:%M]\") entry nil t)
           (concat \"* TODO \" entry \"\\n  Added: \"
                   (format-time-string \"[%Y-%m-%d %a %H:%M]\") \"\\n\"))
         \"\\n\")
        (save-buffer))
      file)"
    ```

    ## Subitems

    If the entry is a project rather than a single action, write the whole
    heading tree into the entry file yourself, with the parent heading first:

    ```
    * TODO Bring up the new router
      Added: %now%
      See [[file:~/plans/router.org]] for the details.
    ** TODO First action
    ** TODO Second action
    ```

    - Use `**` for subitems; org ignores everything after the first heading.
    - Write `Added: %now%` on the parent heading. Elisp replaces `%now%` with
      the current timestamp. Do not try to stamp it with `sed` or `date` —
      `date`'s format string is not portable and the backrefs bite.
    - Keep `Added: [...]` on the parent only — that is the capture convention.
    - Split the tree across `**` subitems when the effort has independently
      finishable steps; keep a flat `* TODO` when it is one action.
    - Do not duplicate the plan into org. One subitem per phase, with a link to
      the plan doc so there is a single source of truth.

    ## Rules

    - Never edit the journal files with shell redirection while Emacs may have them open; use `emacsclient`.
    - Always create `~/org/agenda/` if it does not exist.
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

in
[
  {
    type = "skill";
    name = "disk-space";
    description = "Find local files and caches that are candidates for deletion to save disk space. Use when the user asks to free space, clean up disk, find large files, audit caches, or prune containers.";
    extraFrontmatter = {
      "allowed-tools" = "Bash, Read, Grep, Glob";
    };
    body = skillBody ./skills/disk-space/SKILL.md;
  }

  {
    type = "skill";
    name = "explain-diff-html";
    description = "Use when the user asks for a rich explanation of a code change, diff, branch, or PR. Produces HTML output.";
    body = skillBody ./skills/explain-diff-html/SKILL.md;
  }

  {
    type = "skill";
    name = "grilling";
    description = "Interview the user relentlessly about a plan or design. Use when the user wants to stress-test a plan before building, or uses any grill trigger phrases.";
    body = skillBody ./skills/grilling/SKILL.md;
  }

  {
    type = "skill";
    name = "teach";
    description = "Teach the user a new skill or concept, within this workspace.";
    extraFrontmatter = {
      "disable-model-invocation" = true;
      "argument-hint" = "What would you like to learn about?";
    };
    body = skillBody ./skills/teach/SKILL.md;
    files = {
      "GLOSSARY-FORMAT.md".text = builtins.readFile ./skills/teach/GLOSSARY-FORMAT.md;
      "LEARNING-RECORD-FORMAT.md".text = builtins.readFile ./skills/teach/LEARNING-RECORD-FORMAT.md;
      "MISSION-FORMAT.md".text = builtins.readFile ./skills/teach/MISSION-FORMAT.md;
      "RESOURCES-FORMAT.md".text = builtins.readFile ./skills/teach/RESOURCES-FORMAT.md;
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
    name = "org-agenda-todo";
    description = "Add a TODO item to ~/org/agenda/inbox.org or ~/org/agenda/<category>.org using emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
      "disable-model-invocation" = true;
      "argument-hint" = "[category] TODO text";
    };
    body = orgAgendaTodoBody;
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

  {
    type = "skill";
    name = "org-agenda-pick";
    description = "Pick a TODO item from the Emacs org-agenda, change its TODO state, add sub-items, and link it to a Denote note via emacsclient.";
    extraFrontmatter = {
      tools = "Bash";
    };
    body = skillBody ./skills/org-agenda-pick/SKILL.md;
    files."agent-skill-org-agenda.el".text =
      builtins.readFile ./skills/org-agenda-pick/agent-skill-org-agenda.el;
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

  {
    type = "skill";
    name = "open-code-review-delegate";
    description = "Delegation mode for open-code-review (OCR): host agent reviews; OCR only does file selection and rule resolution.";
    extraFrontmatter = {
      license = "Apache-2.0";
    };
    # Source: BSFishy/nix-config @ fd3cf7f (Apache-2.0)
    body = skillBody ./skills/open-code-review-delegate/SKILL.md;
  }
]
