---
name: org-agenda-pick
description: Pick a TODO from the user's Emacs org-agenda, change its TODO state, add sub-items, and link it to a Denote note via emacsclient.
tools: "Bash"
---

# Pick and update org-agenda TODOs

Work one entry at a time: list, pick, edit. Everything runs through
`emacsclient` so Org owns the file, the LOGBOOK state change and any open buffer.

`agent-skill-org-agenda.el` lives next to this file. All arguments are
**positional** (this Emacs mis-handles multi-key `&key` arglists); pass `nil` to
skip an argument.

```sh
emacsclient --eval '
(progn
  (load (expand-file-name "~/.pi/agent/skills/org-agenda-pick/agent-skill-org-agenda.el") nil t)
  (agent-skill-org-agenda-list))'
```

## 1. List candidates

`(agent-skill-org-agenda-list &optional FILE KEYWORD TAG LIMIT)`

- `FILE`: one agenda file (`"workiva.org"`, or a path). Default: all of `org-agenda-files`, newest modified first.
- `KEYWORD`: substring of the heading, case-insensitive.
- `TAG`: org tag, case-insensitive.
- `LIMIT`: rows shown (default 30); the total match count is reported.

Output rows are `file.org:line  STATE  title  :tags:`, sub-items indented.
A `BLOCKED` flag means open children must be finished first.

## 2. Edit the chosen entry

```sh
emacsclient --eval '
(progn
  (load (expand-file-name "~/.pi/agent/skills/org-agenda-pick/agent-skill-org-agenda.el") nil t)
  (agent-skill-org-agenda-edit
   "workiva.org" 263 "rotate the API key"
   (list "rotate in vault" "update CI secrets")
   "DOING"
   "/Users/yuan/org/denote/20260103T090000--api-key-rotation__wk.org"))'
```

`(agent-skill-org-agenda-edit &optional FILE LINE HEADING SUBITEMS STATE NOTE)`

- `FILE`, `LINE`: the entry to edit, from the listing.
- `HEADING`: its title from the listing. Required: the edit aborts if the line no
  longer holds that heading, so line numbers can never hit the wrong entry.
- `SUBITEMS`: list of strings, appended as `** TODO` children of the entry. `nil` for none.
- `STATE`: a TODO keyword: `TODO DOING WAIT DEFER TASK HABIT DONE CANCELED` (or `NOTE LINK FEEDBACK`).
- `NOTE`: a Denote note path or identifier, linked into the entry as
  `- [[denote:ID][title]]`.

## 3. Notes

- Create the note first with the `denote-note` skill, then pass the path it printed as `NOTE`. Do not invent a Denote identifier.
- Link one entry per call. To link a sub-item to a different note, list again and
  call `edit` with that sub-item's line.
- Sub-items go at the end of the entry's subtree, so a `DONE` on a parent with open
  children may report a dependency error. Add sub-items before closing an entry.
- Never hand-edit the agenda files. If `emacsclient` fails, ask the user to run
  `M-x server-start`.

## 4. Reporting

Say which entry changed, its new state, what was added, and the note linked.
