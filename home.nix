{ config, pkgs, lib, ... }:


let
  user-config = "${config.home.homeDirectory}/projects/user-config";
  link = config.lib.file.mkOutOfStoreSymlink;
  linkConfig = path: link "${user-config}/${path}";
in
{
  imports = [
    ./emacs.nix
    ./xsession.nix
    ./calendar.nix
  ];
  _module.args = {
    inherit user-config link linkConfig;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.keyboard.options = [
    "ctrl:nocaps"
    "altwin:swap_lalt_lwin"
  ];


  home.username = "shou";
  home.homeDirectory = "/home/shou";

  programs.git = {
    enable = true;
    includes = [ { path = ./base/gitconfig; } ];
    ignores = [ (builtins.readFile ./base/gitignore) ];
  };

  programs.ssh = {
    enable = true;
    includes = [ (toString ./base/ssh_config.private) ];
  };

  programs.emacs.enable = true;
  xdg.enable = true;

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      emoji = ["Noto Color Emoji"];
    };
  };

  home.packages = with pkgs; [
    # apps
    alacritty
    calibre
    firefox
    starship

    # tools
    (nvtopPackages.nvidia.override { amd = true; })

    # shell utils
    git-crypt
    zoxide

    # dev tools
    nixd # nix lsp
  ];


  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  home.stateVersion = "24.11";
}
