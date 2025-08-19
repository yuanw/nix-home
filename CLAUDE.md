# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Nix flake-based configuration repository for managing macOS (using nix-darwin) and NixOS systems declaratively. It uses flake-parts for modular flake organization and supports multiple host configurations.

## Common Commands

### Building Systems
- `just build` - Build the system (defaults to wk01174)
- `nix build .#<hostname>` - Build specific host configuration
  - Available hosts: `asche`, `misfit`, `yuanw`, `ci`, `wk01174`, `mist`

### Updates
- `just update-all` - Update all flake inputs
- `just update <INPUT>` - Update specific flake input

### System Management
- `./result/sw/bin/darwin-rebuild switch --flake .` - Apply macOS configuration
- `nixos-rebuild switch --flake '.#' --use-remote-sudo` - Apply NixOS configuration
- `darwin-rebuild --rollback` - Rollback macOS changes
- `nix-diff /run/current-system ./result` - See differences between current and built system

### Formatting and Linting
- `nix fmt` - Format all files using treefmt
- Available formatters: deadnix, nixfmt, ormolu, cabal-fmt, hlint, shellcheck, shfmt
- Pre-commit hooks are configured via flake-parts

### Development Shell
- `nix develop` - Enter development shell with tools

## Architecture

### Core Structure
- `flake.nix` - Main flake configuration using flake-parts
- `hosts/` - Host-specific configurations
  - `hosts/default.nix` - Defines nixosConfigurations and darwinConfigurations
  - Each host has its own subdirectory with configuration files
- `modules/` - Shared modules organized by functionality
  - `modules/default.nix` - Module registry with common, linux, and darwin imports
  - Platform-specific modules (e.g., `brew.nix` for macOS, `qmk.nix` for Linux)
  - Development environment modules in `dev/`
  - Editor configurations in `editor/`
  - Window manager configs in `wm/`

### Key Flake Inputs
- `nixpkgs` (unstable), `nixpkgs-stable` (25.05), `nixpkgs-master`
- `nix-darwin` for macOS system management
- `home-manager` for user environment management
- `agenix` for secrets management
- `catppuccin` for theming
- `treefmt-nix` and `pre-commit` for code formatting

### Host Configuration Pattern
The repository supports multiple host types:
- **NixOS hosts**: `asche`, `misfit` - Linux workstations/servers
- **Darwin hosts**: `yuanw`, `ci`, `mist`, `WK01174` - macOS machines with different privacy/work configurations

### Module System
Modules are organized by:
- Platform compatibility (common, linux-only, darwin-only)
- Functionality (development tools, editors, window managers, browsers)
- Each module is self-contained and can be enabled/disabled per host

### Secrets Management
- Uses `agenix` for encrypted secrets
- Secrets stored in `secrets/` directory
- GPG keys required for decryption during builds

## Development Workflow

1. Make changes to modules or host configurations
2. Test locally with `just build` or `nix build .#<hostname>`
3. Use `nix fmt` to format code
4. Apply changes with appropriate rebuild command
5. Use `nix-diff` to verify expected changes

## Bootstrap Process

For new macOS systems, follow the bootstrap process in `readme.org`:
1. Install Xcode command line tools
2. Install Nix with determinate installer
3. Install Homebrew (managed via nix-darwin)
4. Configure GPG keys for secrets
5. Build and switch to configuration