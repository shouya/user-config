{ config, pkgs, lib, linkConf, ... }:
{
  i18n.inputMethod = {
    enabled = "fcitx5";
    fcitx5.addons = with pkgs; [
      fcitx5-chewing
      fcitx5-hangul
      fcitx5-chinese-addons # opencc, etc
    ];
  };

  xdg.configFile."fcitx5".source = linkConf "fcitx5";
}
