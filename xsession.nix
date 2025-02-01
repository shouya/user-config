{ config, pkgs, lib, linkConfig, ... }:
{
  home.file = {
    ".Xresources".source = linkConfig "x11/Xresources.herbian";
    ".config/picom".source = linkConfig "xdg/picom";

    ".config/copyq/copyq.conf".source = linkConfig "xdg/copyq/copyq.conf";
    ".config/copyq/copyq-commands.conf".source = linkConfig "xdg/copyq/copyq-commands.conf";
  };

  xsession.enable = true;
  xsession.importedVariables = [
    "PATH" # allow tools
  ];
  xsession.windowManager.command = "env SHLVL=0 ${pkgs.xmonad-with-packages}/bin/xmonad";

  home.packages = with pkgs; [
    # fonts
    cantarell-fonts # used on ui
    jetbrains-mono # used in emacs
    nerd-fonts.symbols-only # used by eww
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts # used by system

    # desktop session
    autocutsel
    gnome-keyring # ssh-agent, etc
    libnotify # for notify-send
    networkmanagerapplet # nm-applet
    picom
    rofi
    gcr # provides org.gnome.keyring.SystemPrompter (pinentry?)
    seahorse # manage gnome keyring
  ];

  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
  };
  systemd.user.services.eww = {
      Unit = {
        Description = "Eww";
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart = "${pkgs.eww}/bin/eww --no-daemonize daemon";
      Install.WantedBy = [ "graphical-session.target" ];
  };
  systemd.user.services.eww-open-main-window = {
    Unit = {
      Description= "Eww Open Main Window";
      Requires = [ "eww.service" ];
      After = [ "eww.service" ];
    };

    Service.Type = "oneshot";
    Service.ExecStart = "${pkgs.eww}/bin/eww open --no-daemonize main-window";
    Service.ExecStop = "${pkgs.eww}/bin/eww close main-window";
    Service.RemainAfterExit = true;

    Install.WantedBy = [ "graphical-session.target" ];
  };

  programs.alacritty.enable = true;
  programs.alacritty.settings = {
    window.dimensions = {
      lines = 24;
      columns = 80;
    };
    env.REAL_TERM = "alacritty";
    env.TERM = "xterm-256color";
    keyboard.bindings = [
      { mods = "Super";   key = "Equals"; action = "IncreaseFontSize"; }
      { mods = "Super";   key = "Minus";  action = "DecreaseFontSize"; }
      { mods = "Control"; key = "Equals"; action = "None";             }
      { mods = "Control"; key = "Minus";  action = "None";             }
    ];
  };

  services.gnome-keyring.enable = true;
  services.copyq.enable = true;
  services.pasystray.enable = true;
  services.udiskie.enable = true;

  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
  };
}
