{ config, pkgs, lib, inputs, linkConfig, ... }:
{
  # comma: run commands without installing
  programs.nix-index-database.comma.enable = true;

  # shell utils
  home.packages = with pkgs; [
    starship
    zoxide
    openssl # generate random password
  ];

  # tmux
  xdg.configFile."tmux/tmux.conf".source = linkConfig "xdg/tmux/tmux.conf";

  # direnv
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
