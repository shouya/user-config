{ config, pkgs, lib, scripts, linkConf, wrapGL, ... }:
{
  home.keyboard.options = [
    "ctrl:nocaps"
    "altwin:swap_lalt_lwin"
  ];

  systemd.user.services.setxkbmap.Service.ExecStartPre = ''
  ${pkgs.xorg.xset}/bin/xset r rate 200 45
  '';
}
