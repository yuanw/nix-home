{
  lib,
  agent-bundle,
  janestreet-skills,
  claude-prompts,
  humanizer,
  i-have-adhd,
  emacs-skills,
  cursor-plugins,
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

  # The Emacs skills are captured from upstream's SKILL.md: its frontmatter
  # gives the name and description, its body is the body, and skillFromDir
  # ships the agent-skill-*.el next to it. `extra' replaces the captured
  # frontmatter as a whole: `tools = "Bash"` rides along (upstream omits it in
  # `file-links' — there `tools = null' drops the key), and pi wants
  # `disable-model-invocation' as a real true even though the capture parses
  # it as one — the explicit form keeps the rendered SKILL.md identical to
  # the previous rendering.
  emacsSkill =
    name: extra:
    agent-bundle.skillFromDir {
      dir = "${emacs-skills}/skills/${name}";
      extraFrontmatter = {
        tools = "Bash";
      }
      // extra;
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

  # Captured straight from upstream's SKILL.md: agent-bundle reads the name
  # and description out of its frontmatter, so there is nothing to keep in sync
  # here. `just update janestreet-skills' picks up upstream changes.
  (agent-bundle.skillFromDir { dir = "${janestreet-skills}/socratic-tutor"; })

  # A single markdown file, so the rest of that repo (its plugin manifests, docs
  # and scripts) stays out of the skills directory. agent-bundle reads name,
  # description (a YAML block scalar) and metadata straight out of the
  # frontmatter, so nothing needs to be restated here.
  (agent-bundle.promptFromMarkdown {
    source = humanizer + /SKILL.md;
  })

  # Captured from upstream's SKILL.md, like socratic-tutor above — the
  # description wraps over two lines and the parser folds it into one.
  (agent-bundle.skillFromDir {
    dir = "${claude-prompts}/skills/caveman";
  })

  # Upstream owns the name, description and body; the frontmatter is restated
  # (disable-model-invocation as a real true, license) because the explicit
  # form keeps the rendered SKILL.md identical to the previous rendering. The
  # agents/ files describe the same skill for other CLIs.
  (agent-bundle.skillFromDir {
    dir = "${i-have-adhd}/skills/i-have-adhd";
    extraFrontmatter = {
      "disable-model-invocation" = true;
      license = "MIT";
    };
    exclude = [
      "agents/gemini.toml"
      "agents/openai.yaml"
    ];
  })

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

  (emacsSkill "d2" { disable-model-invocation = true; })

  (emacsSkill "describe" { disable-model-invocation = true; })

  (emacsSkill "dired" { disable-model-invocation = true; })

  (emacsSkill "emacsclient" { })

  (emacsSkill "file-links" { tools = null; })

  (emacsSkill "gnuplot" { disable-model-invocation = true; })

  (emacsSkill "highlight" { disable-model-invocation = true; })

  (emacsSkill "mermaid" { disable-model-invocation = true; })

  (emacsSkill "open" { disable-model-invocation = true; })

  (emacsSkill "plantuml" { disable-model-invocation = true; })

  (emacsSkill "select" { disable-model-invocation = true; })

  # pstack's comment-hater subagent, captured from cursor/plugins: its
  # frontmatter gives name and description, its body is the persona. Declared
  # as an agent because that is what upstream ships; Pi has no subagent
  # format, so agent-bundle degrades it to a skill. It stays model-invocable
  # (no `disableModelInvocation'): Pi advertises its name, description and
  # path in every session's system prompt and reads the persona only when a
  # task matches — the same description-based delegation Cursor does for its
  # subagent. `/skill:comment-sicko' forces it. Cursor reads the same upstream
  # file straight into ~/.cursor/agents (modules/coding-agents/cursor). `just
  # update cursor-plugins' bumps both.
  (agent-bundle.promptFromMarkdown {
    type = "agent";
    name = "comment-sicko";
    source = cursor-plugins + /pstack/agents/comment-sicko.md;
  })
]
