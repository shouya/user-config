{ config, pkgs, lib, scripts, linkConf, wrapGL, ... }:
{
  imports = [
    ./xmonad.nix
    ./eww.nix
    ./input.nix
  ];

  home.file = {
    ".Xresources".source = linkConf "x11/Xresources.herbian";
    ".config/picom".source = linkConf "picom";
    ".config/copyq/copyq.conf".source = linkConf "copyq/copyq.conf";
    ".config/copyq/copyq-commands.ini".source = linkConf "copyq/copyq-commands.ini";
  };

  xsession.enable = true;
  xsession.importedVariables = [
    "PATH" # allow for calling tools in PATH by eww, vdirsync etc
  ];

  home.packages = with pkgs; [
    # fonts
    cantarell-fonts # used on ui
    jetbrains-mono # used in emacs
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts # used by system

    # desktop session
    autocutsel
    gnome-keyring # ssh-agent, etc
    libnotify # for notify-send
    gcr # provides org.gnome.keyring.SystemPrompter (pinentry?)
    seahorse # manage gnome keyring
    wmctrl # used by emacs, eww, etc

    # used by xmonad
    alsa-utils # amixer, aplay
    playerctl
  ];

  # prefer app indicator (SNI) over legacy tray icons
  xsession.preferStatusNotifierItems = true;

  systemd.user.services.picom = {
    Unit.Description = "Picom X11 Compositor";
    Unit.PartOf = [ "graphical-session.target" ];
    Install.WantedBy = [ "graphical-session.target" ];
    Service.ExecStart = "${wrapGL pkgs.picom}/bin/picom";
    Service.Restart = "always";
    Service.RestartSec = "3";
  };

  programs.alacritty = {
    enable = true;
    package = wrapGL pkgs.alacritty;
    settings = {
      window.dimensions = {
        lines = 24;
        columns = 80;
      };
      env.REAL_TERM = "alacritty";
      env.TERM = "xterm-256color";
      env.WINIT_X11_SCALE_FACTOR = "1.5";
      keyboard.bindings = [
        { mods = "Super";   key = "Equals"; action = "IncreaseFontSize"; }
        { mods = "Super";   key = "Minus";  action = "DecreaseFontSize"; }
        { mods = "Control"; key = "Equals"; action = "None";             }
        { mods = "Control"; key = "Minus";  action = "None";             }
      ];
    };
  };

  programs.rofi = {
    enable = true;
    extraConfig.dpi = 1; # auto dpi
    extraConfig.show-icons = true;
    pass = {
      enable = true;
      extraConfig = ''
      clip=clipboard # save to clipboard instead of primary selection
      default_do=copyPass # don't show menu, just copy
      notify=true # notify on copy success
      '';
    };
  };

  services.gnome-keyring.enable = true;
  services.copyq.enable = true;
  services.pasystray.enable = true;
  services.udiskie.enable = true;
  services.network-manager-applet.enable = true;
  services.dunst.enable = true;

  services.caffeine.enable = true;
  services.xidlehook = {
    enable = true;
    not-when-audio = true;
    not-when-fullscreen = true;
    # In case of bad behaving window setting itself as fullscreen when it's not,
    # find out the culprit with this command:
    # , xwininfo -tree -root | awk '/0x[a-z0-9]{5,}/ {print $1}' | xargs -I9 sh -c '(xprop -id 9 | grep FULL) && xprop -id 9
    environment = {
      output = "$(xrandr | awk '/ primary/{print $1}')";
    };

    timers = [
      {delay = 180;             # 3 min
       command   = ''echo dim;
                     xrandr --output $output --brightness .5'';
       canceller = ''echo undim;
                     xrandr --output $output --brightness 1'';}
      {delay = 60;              # 4 min
       command = ''echo blank;
                   xrandr --output $output --brightness 1;
                   xset dpms force off'';}
      {delay = 360;             # 10 min
       command = "echo sleep; systemctl suspend";}
    ];
  };

  home.sessionPath = [
    "${scripts}/linux"
    "${scripts}/common"
  ];

  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
  };
  gtk.iconTheme = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
  };
}
