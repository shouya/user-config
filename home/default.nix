{ config, pkgs, lib, host, wrapGL, ... }:
let
  root = "${config.home.homeDirectory}/projects/user-config";
  scripts = "${config.home.homeDirectory}/projects/scripts";
  linkConf = path: config.lib.file.mkOutOfStoreSymlink "${root}/conf/${path}";
  link = path: config.lib.file.mkOutOfStoreSymlink "${root}/${path}";
in
{
  imports = [
    ./ssh.nix
    ./emacs.nix
    ./xsession.nix
    ./calendar.nix
    ./fcitx.nix
    ./shell.nix
  ];
  _module.args = {
    inherit scripts linkConf link;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = "shou";
  home.homeDirectory = "/home/shou";

  programs.git = {
    enable = true;
    includes = [ { path = ../conf/gitconfig; } ];
    ignores = [ (builtins.readFile ../conf/gitignore) ];
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
    (wrapGL calibre)
    (wrapGL chromium)
    (wrapGL firefox)
    (wrapGL element)
    (wrapGL telegram-desktop)
    (wrapGL anki-bin)
    (wrapGL mpv) # used by anki
    (wrapGL qalculate-gtk)
    (wrapGL goldendict-ng)

    # tools
    ({
      herbian = nvtopPackages.nvidia;
      mrnix   = nvtopPackages.nvidia.override { amd = true; };
    }."${host}")
    (aider-chat.overrideAttrs (p: {
      version = "0.74.0";
      src = p.src.override { tag = "v0.74.0"; };
    }))
    aria2
    bubblewrap
    cachix
    (wrapGL flameshot)
    delta # git diff
    git-crypt
    kubectl
    pass
    podman
    tmux
    uv
    wireshark
    yt-dlp

    # dev tools
    nixd # nix lsp
  ];

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

  home.stateVersion = "24.11";
}
