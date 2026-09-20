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

  humanizerSrc = pkgs.fetchFromGitHub {
    owner = "blader";
    repo = "humanizer";
    rev = "c78047bd4300e5a995d37ae8c7684aa2d53326cd";
    hash = "sha256-wkrarl0kHUdfQM5pTMikB/yQm0kngmhsMlqoxZ63Fqs=";
  };

  emacsSkillsSrc = pkgs.fetchFromGitHub {
    owner = "xenodium";
    repo = "emacs-skills";
    rev = "de7adccbc4aef5f4e1e7ebc7a487bdcd7f95509a";
    hash = "sha256-ilgWnb3w+6mkeLwy5xkU5iX0NRbguur7iTLVqCu27TA=";
  };

  ponytailSrc = pkgs.fetchFromGitHub {
    owner = "DietrichGebert";
    repo = "ponytail";
    rev = "2ed6c52c9d7e5e56942508591085fd45dea277d3";
    hash = "sha256-bGdXvzhWPwGdz3T2Yh2h6lf+3PBRFAfdBxP5pESmCHI=";
  };

  claudePromptsSrc = pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "claude-prompts";
    rev = "39475306a3462d1ecb4697135b24cfaf6184409c";
    hash = "sha256-AggJ0MAHvUX72xxMeeXZr4h6lmekmyLryrtplI/Am+w=";
  };

  iHaveAdhdSrc = pkgs.fetchFromGitHub {
    owner = "ayghri";
    repo = "i-have-adhd";
    rev = "cbe69fb83c08a37cf54d5ec9ec6bb88c8bc9973c";
    hash = "sha256-56Ia9a8lvALeSmUDAumfu9nzmYBzONSlBpFv7o1w7ys=";
  };

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
      body = skillBody "${emacsSkillsSrc}/skills/${name}/SKILL.md";
    }
    // lib.optionalAttrs (elisp != null) {
      files."${elisp}".text = builtins.readFile "${emacsSkillsSrc}/skills/${name}/${elisp}";
    };

  ponytailSkill = name: description: {
    type = "skill";
    inherit name description;
    extraFrontmatter = {
      license = "MIT";
    };
    body = skillBody "${ponytailSrc}/skills/${name}/SKILL.md";
  };

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
    body = skillBody "${humanizerSrc}/SKILL.md";
  }

  {
    type = "skill";
    name = "caveman";
    description = "Compress and simplify prompts to preserve meaning while reducing use of context.";
    body = skillBody "${claudePromptsSrc}/skills/caveman/SKILL.md";
  }

  {
    type = "skill";
    name = "i-have-adhd";
    description = "Shape output for a reader with ADHD: lead with the next action, number multi-step work, restate state across turns, suppress tangents, give specific time estimates, and make wins visible.";
    extraFrontmatter = {
      "disable-model-invocation" = true;
      license = "MIT";
    };
    body = skillBody "${iHaveAdhdSrc}/skills/i-have-adhd/SKILL.md";
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
