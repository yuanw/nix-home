# External Monitor Arrangement in NixOS (X11)

This guide covers how to configure and arrange external monitors in NixOS using X11.

## Quick Commands

### Check Connected Monitors

```bash
xrandr
```

### Arrange Monitors with xrandr

```bash
# Place HDMI-1 to the right of the primary display
xrandr --output eDP-1 --auto --output HDMI-1 --auto --right-of eDP-1

# Place monitor to the left
xrandr --output HDMI-1 --auto --left-of eDP-1

# Place monitor above
xrandr --output HDMI-1 --auto --above eDP-1

# Place monitor below
xrandr --output HDMI-1 --auto --below eDP-1

# Mirror displays
xrandr --output HDMI-1 --auto --same-as eDP-1

# Set specific resolution and refresh rate
xrandr --output HDMI-1 --mode 3840x2160 --rate 60

# Set specific position (x,y offset from top-left)
xrandr --output HDMI-1 --pos 3840x0

# Disable a monitor
xrandr --output HDMI-1 --off

# Set primary monitor
xrandr --output HDMI-1 --primary
```

## GUI Tools

### arandr (Recommended)

A simple drag-and-drop GUI for arranging monitors:

```bash
nix-shell -p arandr
arandr
```

Features:
- Visual representation of monitor layout
- Drag monitors to arrange them
- Save configurations as shell scripts
- Load saved configurations

### lxrandr

Lightweight alternative:

```bash
nix-shell -p lxrandr
lxrandr
```

## Persistent Configuration with autorandr

autorandr automatically detects connected monitors and applies saved profiles.

### Enable in NixOS

```nix
services.autorandr.enable = true;
```

### Command Line Usage

```bash
# Save current layout as a profile
autorandr --save home

# List saved profiles
autorandr --list

# Load a specific profile
autorandr --load home

# Automatically select and apply a profile
autorandr --change
```

### Declarative Configuration

Define profiles in your NixOS configuration:

```nix
services.autorandr = {
  enable = true;
  defaultTarget = "home";  # Profile to use when no match found

  profiles = {
    # Laptop only (undocked)
    mobile = {
      fingerprint = {
        eDP-1 = "<EDID fingerprint>";
      };
      config = {
        eDP-1 = {
          enable = true;
          primary = true;
          mode = "1920x1080";
          position = "0x0";
          rate = "60.00";
        };
      };
    };

    # Home setup with external monitor
    home = {
      fingerprint = {
        eDP-1 = "<EDID fingerprint>";
        HDMI-1 = "<EDID fingerprint>";
      };
      config = {
        eDP-1 = {
          enable = true;
          mode = "1920x1080";
          position = "0x0";
          rate = "60.00";
        };
        HDMI-1 = {
          enable = true;
          primary = true;
          mode = "3840x2160";
          position = "1920x0";  # To the right of eDP-1
          rate = "60.00";
        };
      };
    };
  };
};
```

### Getting Monitor Fingerprints

To get the EDID fingerprint for your monitors:

```bash
# Method 1: Use autorandr
autorandr --fingerprint

# Method 2: From /sys filesystem
find /sys/devices -name edid -exec xxd {} \; 2>/dev/null

# Method 3: Using xrandr verbose output
xrandr --verbose | grep -A5 "EDID:"
```

## Rotating Monitors

### Using xrandr

```bash
# Rotate 90 degrees clockwise (portrait mode, top on right)
xrandr --output HDMI-1 --rotate right

# Rotate 90 degrees counter-clockwise (portrait mode, top on left)
xrandr --output HDMI-1 --rotate left

# Rotate 180 degrees (upside down)
xrandr --output HDMI-1 --rotate inverted

# Reset to normal orientation
xrandr --output HDMI-1 --rotate normal
```

### Rotation Options

| Option     | Description                              |
|------------|------------------------------------------|
| `normal`   | No rotation (default landscape)          |
| `left`     | Rotate 90° counter-clockwise (portrait)  |
| `right`    | Rotate 90° clockwise (portrait)          |
| `inverted` | Rotate 180° (upside-down landscape)      |

### Combining Rotation with Position

When using a rotated monitor alongside others, account for the new dimensions:

```bash
# Example: Vertical monitor (1080x1920 after rotation) to the left of main display
xrandr --output HDMI-1 --rotate left --mode 1920x1080 --pos 0x0 \
       --output eDP-1 --mode 1920x1080 --pos 1080x0
```

### Declarative Configuration with autorandr

```nix
services.autorandr.profiles.home-vertical = {
  fingerprint = {
    eDP-1 = "<EDID fingerprint>";
    HDMI-1 = "<EDID fingerprint>";
  };
  config = {
    HDMI-1 = {
      enable = true;
      mode = "1920x1080";
      position = "0x0";
      rotate = "left";  # portrait mode
    };
    eDP-1 = {
      enable = true;
      primary = true;
      mode = "1920x1080";
      position = "1080x0";  # to the right of rotated monitor
    };
  };
};
```

### Rotate Touch Input (if applicable)

If using a touchscreen, you may need to rotate the input mapping as well:

```bash
# Find your touch device
xinput list

# Apply rotation matrix for 90° clockwise (right)
xinput set-prop "Your Touch Device" "Coordinate Transformation Matrix" 0 1 0 -1 0 1 0 0 1

# Rotation matrices:
# normal:   1 0 0 0 1 0 0 0 1
# left:     0 -1 1 1 0 0 0 0 1
# right:    0 1 0 -1 0 1 0 0 1
# inverted: -1 0 1 0 -1 1 0 0 1
```

## HiDPI and Scaling

### Per-Monitor DPI (X11 limitations)

X11 has limited support for per-monitor scaling. Options:

```nix
# Set global DPI
services.xserver.dpi = 192;  # For 2x scaling

# Or via Xresources
home-manager.users.myuser = {
  xresources.properties = {
    "Xft.dpi" = 192;
  };
};
```

### Application-Specific Scaling

```bash
# GTK applications
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5

# Qt applications
export QT_AUTO_SCREEN_SCALE_FACTOR=1
# or
export QT_SCALE_FACTOR=2
```

## Troubleshooting

### Monitor Not Detected

```bash
# Force X to re-detect monitors
xrandr --auto

# Check kernel messages
dmesg | grep -i drm
```

### Wrong Resolution Available

```bash
# Add custom mode
xrandr --newmode "2560x1440_60" 312.25 2560 2752 3024 3488 1440 1443 1448 1493 -hsync +vsync
xrandr --addmode HDMI-1 "2560x1440_60"
xrandr --output HDMI-1 --mode "2560x1440_60"
```

### Screen Tearing

Enable compositor or use modesetting driver:

```nix
services.xserver.videoDrivers = [ "modesetting" ];

# Or enable a compositor like picom
services.picom = {
  enable = true;
  vSync = true;
};
```

### autorandr Not Switching Automatically

Ensure udev rules are enabled:

```nix
services.autorandr = {
  enable = true;
  # hooks can be added for custom behavior
  hooks = {
    postswitch = {
      "notify" = "notify-send 'Display' 'Monitor configuration changed'";
    };
  };
};
```

## References

- [ArchWiki - xrandr](https://wiki.archlinux.org/title/Xrandr)
- [ArchWiki - Multihead](https://wiki.archlinux.org/title/Multihead)
- [NixOS Options - autorandr](https://search.nixos.org/options?query=autorandr)
- [autorandr GitHub](https://github.com/phillipberndt/autorandr)
