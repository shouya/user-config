{ pkgs, lib, config, linkConf, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ../conf/xmonad/xmonad.hs;
    extraPackages = hpkgs: [
      # used in eww log
      hpkgs.aeson
    ];
    libFiles = {
      "XMonad/Actions/AbsWS.hs" = ../conf/xmonad/lib/XMonad/Actions/AbsWS.hs;
      "XMonad/Actions/Backlight.hs" = ../conf/xmonad/lib/XMonad/Actions/Backlight.hs;
      "XMonad/Actions/FixedWorkspace.hs" = ../conf/xmonad/lib/XMonad/Actions/FixedWorkspace.hs;
      "XMonad/Actions/Volume.hs" = ../conf/xmonad/lib/XMonad/Actions/Volume.hs;
      "XMonad/Hooks/EwwLog.hs" = ../conf/xmonad/lib/XMonad/Hooks/EwwLog.hs;
      "XMonad/Hooks/SmartFloat.hs" = ../conf/xmonad/lib/XMonad/Hooks/SmartFloat.hs;
      "XMonad/Hooks/TiledProperty.hs" = ../conf/xmonad/lib/XMonad/Hooks/TiledProperty.hs;
      "XMonad/Layout/TallMastersComboModified.hs" = ../conf/xmonad/lib/XMonad/Layout/TallMastersComboModified.hs;
    };
  };

  home.file.".xmonad/scripts".source = ../conf/xmonad/scripts;

  home.file.".xmonad/assets/percussion-10.wav".source =
    ../conf/xmonad/assets/percussion-10.wav;

  xsession.windowManager.command = lib.mkForce ''
  export SHLVL=0
  ~/.xmonad/xmonad-x86_64-linux >/tmp/xmonad.out 2>/tmp/xmonad.err
  '';


  home.sessionVariables.WINDOW_MANAGER = "xmonad";
}
