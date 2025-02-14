{ config, pkgs, lib, inputs, linkConfig, ... }:
{
  # run nix-shell with fish
  programs.nix-your-shell.enable = true;
  home.file.".local/share/fish/vendor_conf.d/nix-your-shell.fish" = {
    text = ''
    ${config.programs.nix-your-shell.package}/bin/nix-your-shell fish | source
    '';
  };

  # shell utils
  home.packages = with pkgs; [
    starship
    zoxide
    openssl # generate random password
  ];

  # fish config
  home.file.".config/fish".source = linkConfig "xdg/fish";

  # tmux
  xdg.configFile."tmux/tmux.conf".source = linkConfig "xdg/tmux/tmux.conf";

  # direnv
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # comma: run commands without installing
  programs.nix-index-database.comma.enable = true;
}
