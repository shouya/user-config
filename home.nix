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
    ./fcitx.nix
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
  systemd.user.services.setxkbmap.Service.ExecStartPre = ''
  ${pkgs.xorg.xset}/bin/xset r rate 200 45
  '';

  home.username = "shou";
  home.homeDirectory = "/home/shou";

  programs.git = {
    enable = true;
    includes = [ { path = ./base/gitconfig; } ];
    ignores = [ (builtins.readFile ./base/gitignore) ];
  };

  home.file.".ssh/config".source = linkConfig "base/ssh_config.private";
  xdg.configFile."tmux/tmux.conf".source = linkConfig "xdg/tmux/tmux.conf";

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
    chromium
    firefox
    element
    telegram-desktop
    anki-bin
    mpv # used by anki
    qalculate-gtk
    goldendict-ng

    # tools
    (nvtopPackages.nvidia.override { amd = true; })
    aider-chat
    aria2
    flameshot
    git-crypt
    kubectl
    pass
    podman
    tmux
    uv
    wireshark
    yt-dlp

    # shell utils
    starship
    zoxide

    # dev tools
    nixd # nix lsp
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    acceleration = "cuda";
    host = "0.0.0.0";
    environmentVariables = {
      OLLAMA_ORIGINS = "*";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";
    };
  };


  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  home.stateVersion = "24.11";
}
