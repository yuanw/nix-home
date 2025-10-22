# Claude Code Configuration

This directory contains custom Claude Code commands for this Nix configuration repository.

## Available Commands

- `/build-host` - Build a specific host configuration (asche, misfit, yuanw, ci, wk01174, mist)
- `/update-flake` - Update flake inputs (all or specific)
- `/format-nix` - Format all files using treefmt
- `/check-diff` - Show differences between current and built system
- `/rebuild-darwin` - Apply configuration on macOS systems
- `/rebuild-nixos` - Apply configuration on NixOS systems

## Usage

Simply type `/` in Claude Code to see the list of available commands, or type the command name directly.

Example:
```
/build-host
```

## Creating New Commands

To add a new command:
1. Create a new `.md` file in `.claude/commands/`
2. Write the command instructions in plain Markdown
3. Commit to version control
4. The command will be available as `/filename` (without .md)

## Version Control

This directory is committed to version control so all team members have access to the same commands.
