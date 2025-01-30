{ config, pkgs, lib, linkConfig, ... }:
{
  home.packages = with pkgs; [
    khal
    vdirsyncer
  ];

  home.file.".config/vdirsyncer".source = linkConfig "xdg/vdirsyncer";
  home.file.".config/khal".source = linkConfig "xdg/khal";

  systemd.user.timers.vdirsyncer = {
    Unit.Description = "Sync vdirsyncer";
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "15m";
      AccuracySec = "5m";
    };
    Install.WantedBy = [ "timers.target" ];
  };
  systemd.user.services.vdirsyncer = {
    Unit.Description = "Sync vdirsyncer";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
      RuntimeMaxSec = "3m";
      Restart = "on-failure";
    };
  };

}
