{ config, pkgs, lib, linkConf, ... }:
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
      xdotool # for summoning malakal
      eww # eww msg
      config.services.dunst.package # dunstctl
    ];
    eww = pkgs.eww;
in {
  xsession.importedVariables = [
    "WINDOW_MANAGER" # eww uses this to determine if it should show workspaces
  ];

  home.packages = with pkgs; [
    nerd-fonts.symbols-only
    eww
  ];

  xdg.configFile."eww".source = linkConf "eww";

  systemd.user.services.eww = {
      Unit = {
        Description = "Eww";
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart = "${eww}/bin/eww --no-daemonize daemon";
      Service.ExecStartPost = "${eww}/bin/eww open --no-daemonize main-window";
      Service.TimeoutStopSec = "2s";
      Service.Environment = "PATH=${lib.makeBinPath runtimeDeps}";
      Install.WantedBy = [ "graphical-session.target" ];
  };
}
