{ config, pkgs, lib, linkConfig, inputs, system, ... }:
{
  home.packages = with pkgs; [
    khal
    vdirsyncer
    inputs.malakal.defaultPackage.${system}
  ];

  home.file.".config/vdirsyncer".source = linkConfig "xdg/vdirsyncer";
  home.file.".config/khal".source = linkConfig "xdg/khal";
  home.file.".config/malakal/config.toml".source =
    (pkgs.formats.toml {}).generate "config.toml" {
      calendar_name = "time-blocking";
      calendar_location = "~/.calendar/time-blocking";
      notifier_blacklist_processes = ["zoom"];
      post_update_hook = ["${pkgs.eww}/bin/eww" "reload"];
      day_column_width = 500.0;
    };

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
