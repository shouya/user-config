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
    noto-fonts # used by system
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    cantarell-fonts # used on ui
    nerd-fonts.symbols-only # used by eww
    jetbrains-mono # used in emacs

    # desktop session
    xmonad-with-packages
    picom
    networkmanagerapplet # nm-applet
    autocutsel

    # tools
  ];


  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
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
