import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
prefix = sys.argv[2]
theme_name = sys.argv[3]
theme_custom = json.loads(sys.argv[4])
plugin_commands = len(sys.argv) > 5 and sys.argv[5].lower() == "true"
lines = path.read_text().splitlines()
managed_commands = {
    "alonz.command-palette.open",
    "herdr-file-viewer.open-file-viewer",
    "herdr-file-viewer.open-file-viewer-tab",
    "herdr-insight.open-timeline-right",
    "gh-pr.refresh",
    "dutifuldev.ghzinga.open",
    "kkckkchosts.herdr-plugin-gh-workflow.gh-issue-develop",
    "ogulcancelik.github-start.open",
}

filtered = []
i = 0
while i < len(lines):
    if lines[i].strip() == "[[keys.command]]":
        block = [lines[i]]
        i += 1
        while i < len(lines) and not lines[i].strip().startswith("["):
            block.append(lines[i])
            i += 1

        command = None
        for block_line in block:
            stripped = block_line.strip()
            if stripped.startswith("command") and "=" in stripped:
                command = stripped.split("=", 1)[1].strip().strip('"')
                break

        if command in managed_commands:
            continue

        filtered.extend(block)
        continue

    filtered.append(lines[i])
    i += 1

lines = filtered
out = []
in_keys = False
saw_keys = False
managed_keys = {
    "prefix": prefix,
    "settings": "prefix+comma",
    "reload_config": "prefix+ctrl+r",
    "workspace_picker": "prefix+w",
    "new_workspace": "prefix+N",
    "new_worktree": "prefix+g",
    "goto": "prefix+/",
    "open_worktree": "prefix+G",
    "new_tab": "prefix+c",
    "rename_tab": "prefix+alt+t",
    "switch_tab": "prefix+1..9",
    "focus_agent": "prefix+alt+1..9",
    "next_agent": "prefix+J",
    "previous_agent": "prefix+K",
    "previous_tab": "prefix+p",
    "next_tab": "prefix+n",
    "focus_pane_left": "prefix+h",
    "focus_pane_down": "prefix+j",
    "focus_pane_up": "prefix+k",
    "focus_pane_right": "prefix+l",
    "last_pane": "prefix+ctrl+w",
    "cycle_pane_next": "prefix+tab",
    "cycle_pane_previous": "prefix+shift+tab",
    "split_horizontal": "prefix+s",
    "split_vertical": "prefix+v",
    "close_pane": "prefix+x",
    "zoom": "prefix+z",
    "resize_mode": "prefix+r",
    "edit_scrollback": "prefix+enter",
    "toggle_sidebar": "prefix+b",
}
wrote_keys = set()

for line in lines:
    stripped = line.strip()
    if stripped.startswith("[") and stripped.endswith("]"):
        if in_keys:
            for key, value in managed_keys.items():
                if key not in wrote_keys:
                    out.append(f'{key} = "{value}"')
                    wrote_keys.add(key)
        in_keys = stripped == "[keys]"
        saw_keys = saw_keys or in_keys
        out.append(line)
        continue

    if in_keys and "=" in stripped:
        key = stripped.split("=", 1)[0].strip()
        if key in managed_keys:
            if key not in wrote_keys:
                out.append(f'{key} = "{managed_keys[key]}"')
                wrote_keys.add(key)
            continue

    out.append(line)

if saw_keys and in_keys:
    for key, value in managed_keys.items():
        if key not in wrote_keys:
            out.append(f'{key} = "{value}"')
            wrote_keys.add(key)

if not saw_keys:
    if out and out[-1].strip():
        out.append("")
    out.append("[keys]")
    for key, value in managed_keys.items():
        out.append(f'{key} = "{value}"')

command_block = (
    [
        "",
        "[[keys.command]]",
        'key = "prefix+m"',
        'type = "plugin_action"',
        'command = "alonz.command-palette.open"',
        'description = "open command palette"',
        "",
        "[[keys.command]]",
        'key = "prefix+f"',
        'type = "plugin_action"',
        'command = "herdr-file-viewer.open-file-viewer"',
        'description = "open file viewer in a split"',
        "",
        "[[keys.command]]",
        'key = "prefix+F"',
        'type = "plugin_action"',
        'command = "herdr-file-viewer.open-file-viewer-tab"',
        'description = "open file viewer in a tab"',
        "",
        "[[keys.command]]",
        'key = "prefix+T"',
        'type = "plugin_action"',
        'command = "herdr-insight.open-timeline-right"',
        'description = "open agent timeline"',
        "",
        "[[keys.command]]",
        'key = "prefix+R"',
        'type = "plugin_action"',
        'command = "gh-pr.refresh"',
        'description = "refresh GitHub PR status"',
        "",
        "[[keys.command]]",
        'key = "prefix+P"',
        'type = "plugin_action"',
        'command = "dutifuldev.ghzinga.open"',
        'description = "open issue or PR in ghzinga"',
        "",
        "[[keys.command]]",
        'key = "prefix+I"',
        'type = "plugin_action"',
        'command = "kkckkchosts.herdr-plugin-gh-workflow.gh-issue-develop"',
        'description = "start GitHub issue workflow"',
        "",
        "[[keys.command]]",
        'key = "prefix+O"',
        'type = "plugin_action"',
        'command = "ogulcancelik.github-start.open"',
        'description = "start from GitHub item"',
    ]
    if plugin_commands
    else []
)

if command_block:
    if out and out[-1].strip():
        out.append("")
    out.extend(command_block[1:])


def upsert_worktree_directory(lines):
    out = []
    in_worktrees = False
    saw_worktrees = False
    wrote_directory = False

    for line in lines:
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            if in_worktrees and not wrote_directory:
                out.append('directory = "~/.local/share/herdr/worktrees"')
            in_worktrees = stripped == "[worktrees]"
            saw_worktrees = saw_worktrees or in_worktrees
            out.append(line)
            continue

        if in_worktrees and "=" in stripped:
            key = stripped.split("=", 1)[0].strip()
            if key == "directory":
                if not wrote_directory:
                    out.append('directory = "~/.local/share/herdr/worktrees"')
                    wrote_directory = True
                continue
            if key == "post_create_command":
                continue

        out.append(line)

    if saw_worktrees and in_worktrees and not wrote_directory:
        out.append('directory = "~/.local/share/herdr/worktrees"')
    elif not saw_worktrees:
        if out and out[-1].strip():
            out.append("")
        out.extend(
            [
                "[worktrees]",
                'directory = "~/.local/share/herdr/worktrees"',
            ]
        )

    return out


def upsert_simple_section(lines, section, managed_values):
    out = []
    in_section = False
    saw_section = False
    wrote = set()

    header = f"[{section}]"
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            if in_section:
                for key, value in managed_values.items():
                    if key not in wrote:
                        out.append(f"{key} = {value}")
                        wrote.add(key)
            in_section = stripped == header
            saw_section = saw_section or in_section
            out.append(line)
            continue

        if in_section and "=" in stripped:
            key = stripped.split("=", 1)[0].strip()
            if key in managed_values:
                if key not in wrote:
                    out.append(f"{key} = {managed_values[key]}")
                    wrote.add(key)
                continue

        out.append(line)

    if saw_section and in_section:
        for key, value in managed_values.items():
            if key not in wrote:
                out.append(f"{key} = {value}")
                wrote.add(key)
    elif not saw_section:
        if out and out[-1].strip():
            out.append("")
        out.append(header)
        for key, value in managed_values.items():
            out.append(f"{key} = {value}")

    return out


def replace_section(lines, header, body_lines):
    out = []
    in_target = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            in_target = stripped == header
            if in_target:
                continue
            out.append(line)
            continue
        if in_target:
            continue
        out.append(line)

    if body_lines:
        if out and out[-1].strip():
            out.append("")
        out.append(header)
        out.extend(body_lines)
    return out


out = upsert_worktree_directory(out)
out = upsert_simple_section(out, "session", {"resume_agents_on_restore": "true"})
out = upsert_simple_section(out, "experimental", {"pane_history": "true"})
out = upsert_simple_section(
    out,
    "ui",
    {
        "agent_panel_sort": '"priority"',
        "hide_tab_bar_when_single_tab": "true",
        "prompt_new_tab_name": "false",
    },
)
out = replace_section(out, "[theme]", [f'name = "{theme_name}"'])
out = replace_section(
    out,
    "[theme.custom]",
    [f'{k} = "{v}"' for k, v in theme_custom.items()],
)

path.write_text("\n".join(out) + "\n")
