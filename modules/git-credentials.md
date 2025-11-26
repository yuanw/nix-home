# Git Credentials Module

This module provides a unified interface for configuring Git credential helpers in NixOS.

## Usage

Enable the module in your host configuration:

```nix
modules.git-credentials = {
  enable = true;
  helper = "oauth";  # or "manager", "libsecret", "pass"
};
```

## Available Helpers

### OAuth (Recommended)

Uses `git-credential-oauth` for OAuth-based authentication with GitHub, GitLab, and other services.

```nix
modules.git-credentials = {
  enable = true;
  helper = "oauth";
};
```

**First-time setup:**
1. When you first push/pull, it will open a browser for OAuth authentication
2. Credentials are stored securely in your system keyring
3. Works great with 2FA and personal access tokens

**Best for:** GitHub, GitLab, Bitbucket

### Git Credential Manager

Uses Microsoft's cross-platform Git Credential Manager (requires .NET runtime).

```nix
modules.git-credentials = {
  enable = true;
  helper = "manager";
};
```

**Best for:** Azure DevOps, GitHub Enterprise

### libsecret

Uses GNOME Keyring / libsecret for credential storage.

```nix
modules.git-credentials = {
  enable = true;
  helper = "libsecret";
};
```

**Requires:** GNOME desktop environment or gnome-keyring daemon running
**Best for:** GNOME/GTK desktop users

### Pass Password Manager

Uses the Unix `pass` password manager for credential storage.

```nix
modules.git-credentials = {
  enable = true;
  helper = "pass";
  passMapping = ''
    [github.com*]
    target=github

    [gitlab.com*]
    target=gitlab
  '';
};
```

**Requires:** You must have passwords stored in pass at the specified paths
**Best for:** Users already using pass for password management

**Setup:**
1. Initialize pass: `pass init <gpg-key-id>`
2. Store credentials: `pass insert github` (username on first line, token on second)
3. Configure mapping as shown above

## Current Configuration

- **asche**: Uses `pass` helper with GitHub mapping
- **misfit**: Uses `oauth` helper for easy GitHub/GitLab access

## References

- [git-credential-oauth](https://github.com/hickford/git-credential-oauth)
- [Git Credential Manager](https://github.com/git-ecosystem/git-credential-manager)
- [pass-git-helper](https://github.com/languitar/pass-git-helper)
- [NixOS Wiki - Git](https://nixos.wiki/wiki/Git)
