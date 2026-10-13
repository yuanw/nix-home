{ ... }:

{
  epkgs = epkgs: [
    epkgs.pilish
  ];

  # Pilish reads pi session JSONL under ~/.pi/agent/sessions (or
  # $PI_CODING_AGENT_DIR/sessions).  Session/tree browsers need no live
  # process; opening a session as a live chat still shells out to `pi`.
  # Bound under the existing pi-opener prefix (`C-c q`), next to the
  # agent-shell helpers in tempel.el.
  elisp = ''
    (use-package pilish
      :commands (pilish
                 pilish-toggle
                 pilish-session-browser
                 pilish-tree-browser
                 pilish-open-session-file)
      :bind (("C-c q s" . pilish-session-browser)
             ("C-c q t" . pilish-tree-browser)
             ("C-c q o" . pilish-open-session-file)))
  '';
}
