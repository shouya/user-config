{ config, pkgs, lib, linkConfig, ... }:
{
  home.file = {
    ".xsession".source = linkConfig "x11/xsession";
    ".Xresources".source = linkConfig "x11/Xresources.herbian";
    ".config/picom".source = linkConfig "xdg/picom";
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
    picom

    # tools
  ];


  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
  };

  home.pointerCursor = {
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
  };
}
