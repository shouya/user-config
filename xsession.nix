{ config, pkgs, lib, linkConfig, ... }:
{
  home.file = {
    ".xsession".source = linkConfig "x11/xsession";
    ".Xresources".source = linkConfig "x11/Xresources.herbian";
  };

  home.packages = with pkgs; [
    # fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    cantarell-fonts
    nerd-fonts.fira-code
  ];


  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
  };

  # home.pointerCursor.size = 64;
}
