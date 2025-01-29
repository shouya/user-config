{ config, pkgs, lib, ... }:

let
  user-config = "${config.home.homeDirectory}/projects/user-config";
  link = config.lib.file.mkOutOfStoreSymlink;
  linkConfig = path: link "${user-config}/${path}";
in
{
  imports = [
    ./emacs.nix
  ];
  _module.args = {
    inherit user-config link linkConfig;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = "shou";
  home.homeDirectory = "/home/shou";

  home.packages = [
  ];

  home.file = {
    ".xsession".source = ./x11/xsession;
    ".Xresources".source = ./x11/Xresources.herbian;
  };

  programs.git = {
    enable = true;
    includes = [ { path = ./base/gitconfig; } ];
    ignores = [ (builtins.readFile ./base/gitignore) ];
  };

  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
  };

  programs.emacs.enable = true;
  xdg.enable = true;


  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  home.stateVersion = "24.11"; # Please read the comment before changing.
}
