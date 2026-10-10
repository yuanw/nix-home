"""Convert a JSON description of UCI sections into ``uci batch`` commands.

Vendored from Mic92's nix-uci (MIT), from
https://github.com/Mic92/dotfiles/tree/main/openwrt. Adaptations here:

* secrets come from a plain JSON object named by ``$NIX_UCI_SECRETS`` instead of
  a sops file, so the plaintext never lands in the nix store and 1Password can
  fill it from a ``just`` recipe;
* a secret that resolves to the empty string is a hard error -- ``op read`` of a
  present-but-blank 1Password field exits 0 with no output, which would otherwise
  silently produce an open access point.
"""

import json
import os
import re
import sys

class ConfigError(Exception):
    """The declared configuration is not usable."""


# uci verbs that mean something inside a batch. Verified against this box:
# `uci help` lists add_list/batch/commit/del_list/delete/rename/reorder; set and
# add are implied. commit/revert belong to the recipes, not the config.
RAW_VERBS = frozenset(
    {"add", "add_list", "del_list", "delete", "rename", "reorder", "set"}
)


def load_secrets() -> dict[str, str]:
    path = os.environ.get("NIX_UCI_SECRETS")
    if not path:
        return {}
    try:
        with open(path) as handle:
            secrets = json.load(handle)
    except OSError as exc:
        raise ConfigError(f"cannot read $NIX_UCI_SECRETS={path}: {exc}") from exc
    except json.JSONDecodeError as exc:
        raise ConfigError(f"$NIX_UCI_SECRETS={path} is not valid JSON: {exc}") from exc
    if not isinstance(secrets, dict):
        raise ConfigError(f"{path} must contain a JSON object")
    return secrets


def interpolate(value: str, secrets: dict[str, str]) -> str:
    def substitute(match: re.Match) -> str:
        name = match.group(1)
        if name not in secrets:
            msg = f"secret {name!r} is not defined (is $NIX_UCI_SECRETS set?)"
            raise ConfigError(msg)
        secret = secrets[name]
        if secret == "":
            msg = f"secret {name!r} resolved to an empty string"
            raise ConfigError(msg)
        return str(secret)

    return re.sub(r"@(.*?)@", substitute, str(value))


def serialize_option(key: str, value: object, secrets: dict[str, str]) -> list[str]:
    if isinstance(value, bool | int | float | str):
        return [f"set {key}='{interpolate(value, secrets)}'"]
    if isinstance(value, list):
        return [f"add_list {key}='{interpolate(v, secrets)}'" for v in value]
    msg = f"{key}: {value!r} is not a scalar or a list"
    raise ConfigError(msg)


def serialize_anonymous(
    config: str, name: str, index: int, section: dict, secrets: dict[str, str]
) -> list[str]:
    section = dict(section)  # don't mutate the caller's dict
    if "_type" not in section:
        msg = f"{config}.@{name}[{index}] has no _type"
        raise ConfigError(msg)
    lines = [f"set {config}.@{name}[{index}]={section.pop('_type')}"]
    for option, value in section.items():
        lines.extend(serialize_option(f"{config}.@{name}[{index}].{option}", value, secrets))
    return lines


def serialize_named(
    config: str, name: str, section: dict, secrets: dict[str, str]
) -> list[str]:
    section = dict(section)
    # Deleting the whole section first means undeclared options really go away.
    lines = [f"delete {config}.{name}"]
    if "_type" not in section:
        msg = f"{config}.{name} has no _type"
        raise ConfigError(msg)
    lines.append(f"set {config}.{name}={section.pop('_type')}")
    for option, value in section.items():
        lines.extend(serialize_option(f"{config}.{name}.{option}", value, secrets))
    return lines


def serialize(configs: dict, secrets: dict[str, str]) -> str:
    if not isinstance(configs, dict):
        msg = f"settings must be a dict of configs, got {type(configs).__name__}"
        raise ConfigError(msg)
    lines: list[str] = []
    for config, sections in configs.items():
        if not isinstance(sections, dict):
            msg = f"{config}: expected a dict of sections, got {type(sections).__name__}"
            raise ConfigError(msg)
        for name, section in sections.items():
            if isinstance(section, list):
                # No delete commands here: an anonymous section cannot be targeted
                # by index reliably, because uci renumbers what is left after each
                # delete, so a fixed index loop hits empty slots and leaves stale
                # options merged into what gets re-added. The caller wipes the type
                # first with `while uci -q delete c.@type[0]; do :; done` -- that
                # deletes the whole type however many sections there are, and is
                # driven by the `add` lines below. See docs/openwrt-plan.org.
                if not section:
                    continue
                for _ in range(len(section)):
                    lines.append(f"add {config} {name}")
                for index, entry in enumerate(section):
                    if not isinstance(entry, dict):
                        msg = f"{config}.@{name}[{index}]: expected a section"
                        raise ConfigError(msg)
                    lines.extend(serialize_anonymous(config, name, index, entry, secrets))
            elif isinstance(section, dict):
                lines.extend(serialize_named(config, name, section, secrets))
            else:
                msg = f"{config}.{name}: expected a dict or a list of dicts"
                raise ConfigError(msg)
    return "\n".join(lines)


def convert(path: str, secrets: dict[str, str]) -> str:
    with open(path) as handle:
        data = json.load(handle)
    settings = data.get("settings")
    if not isinstance(settings, dict):
        msg = f"{path}: no 'settings' object"
        raise ConfigError(msg)
    if not settings:
        msg = f"{path}: settings is empty, refusing to emit nothing"
        raise ConfigError(msg)
    return "\n".join(
        [*serialize(settings, secrets).splitlines(), *raw_commands(data.get("raw", []))]
    )


def raw_commands(raw: list) -> list[str]:
    """Escape hatch: uci commands the typed model cannot express, appended after
    the declared config so they can fix section order or rename one-off leftovers.

    Only verbs that do something useful inside a batch are allowed, which also
    rejects shell: a batch is read by uci, not by a shell, so `rm -rf /` here is a
    string, and `commit`/`revert` would undo the whole point of reviewing first.
    """
    if not isinstance(raw, list):
        msg = "raw: expected a list of uci commands"
        raise ConfigError(msg)
    lines = []
    for command in raw:
        if not isinstance(command, str):
            msg = "raw: each command must be a string"
            raise ConfigError(msg)
        verb = command.split(" ", 1)[0]
        if verb not in RAW_VERBS:
            msg = (
                f"raw: {command!r}: first word must be one of "
                f"{' '.join(sorted(RAW_VERBS))}; commands run inside `uci batch`, so "
                "no `uci` prefix, no commit/revert/show"
            )
            raise ConfigError(msg)
        lines.append(command)
    return lines


def main() -> None:
    if len(sys.argv) != 2:
        print(f"USAGE: {sys.argv[0]} UCI_JSON", file=sys.stderr)
        sys.exit(2)
    try:
        print(convert(sys.argv[1], load_secrets()))
    except ConfigError as exc:
        print(f"nix-uci: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
