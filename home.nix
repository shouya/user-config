{ config, pkgs, ... }:

let user-config = ./..;
in
{
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

  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  home.stateVersion = "24.11"; # Please read the comment before changing.
}
