{ config, pkgs, lib, linkConfig, ... }:
{
  home.file = {
    ".xsession".source = linkConfig "x11/xsession-nix";
    ".Xresources".source = linkConfig "x11/Xresources.herbian";
    ".config/picom".source = linkConfig "xdg/picom";

    ".config/copyq/copyq.conf".source = linkConfig "xdg/copyq/copyq.conf";
    ".config/copyq/copyq-commands.conf".source = linkConfig "xdg/copyq/copyq-commands.conf";

  };

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
    xmonad-with-packages
  ];

  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
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

  services.copyq.enable = true;
  services.pasystray.enable = true;
  services.udiskie.enable = true;

  # https://github.com/nix-community/home-manager/issues/2064#issuecomment-887300055
  # required by pasystray
	systemd.user.targets.tray = {
		Unit = {
			Description = "Home Manager System Tray";
			Requires = [ "graphical-session-pre.target" ];
		};
	};

  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
  };
}
