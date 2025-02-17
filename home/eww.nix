{ config, pkgs, lib, linkConf, wrapGL, ... }:
let eww = pkgs.symlinkJoin {
      name = "eww";
      paths = [ (wrapGL pkgs.eww) ];
      buildInputs = [ pkgs.makeWrapper ];
      # make sure eww widgets can call runtime deps and eww binary.
      postBuild = ''
        wrapProgram $out/bin/eww \
          --prefix PATH : ${lib.makeBinPath runtimeDeps} \
          --prefix PATH : $out/bin \
          --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [pkgs.xapp]}
      '';
    };
    runtimeDeps  = with pkgs; [
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
      config.services.dunst.package # dunstctl
    ];
in {
  xsession.importedVariables = [
    "WINDOW_MANAGER" # eww uses this to determine if it should show workspaces
  ];

  home.packages = with pkgs; [
    nerd-fonts.symbols-only
  ] ++ [eww];

  xdg.configFile."eww".source = linkConf "eww";

  systemd.user.services.eww = {
      Unit = {
        Description = "Eww";
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart = "${eww}/bin/eww --no-daemonize daemon";
      Service.ExecStartPost = "${eww}/bin/eww open main-window";
      Service.TimeoutStopSec = "2s";
      Install.WantedBy = [ "graphical-session.target" ];
  };
}
