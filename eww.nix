{ config, pkgs, lib, linkConfig, ... }:
let runtimeDeps  = with pkgs; [
      bash
      coreutils # stdbuf
      gawk # awk
      gnugrep # grep
      gnused # sed
      iproute2 # ip
      khal
      procps # pgrep
      pulseaudio # pactl
      python313Full
      util-linux # cal
      wirelesstools # iwgetid
      wmctrl
      xorg.xprop # xprop
    ];
    eww = pkgs.eww;
in {
  xsession.importedVariables = [
    "WINDOW_MANAGER" # eww uses this to determine if it should show workspaces
  ];

  home.packages = with pkgs; [
    nerd-fonts.symbols-only
  ];

  programs.eww = {
    enable = true;
    configDir = ./xdg/eww;
    package = eww;
  };

  systemd.user.services.eww = {
      Unit = {
        Description = "Eww";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart = "${eww}/bin/eww --no-daemonize daemon";
      Service.ExecStartPost = "${eww}/bin/eww open --no-daemonize main-window";
      Service.Environment = "PATH=${lib.makeBinPath runtimeDeps}";
      Install.WantedBy = [ "graphical-session.target" ];
  };
}
