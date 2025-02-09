{ pkgs, lib, config, linkConfig, ... }:
{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xdg/xmonad/xmonad.hs;
    extraPackages = hpkgs: [
      # used in eww log
      hpkgs.aeson
    ];
    libFiles = {
      "XMonad/Actions/AbsWS.hs" = ./xdg/xmonad/lib/XMonad/Actions/AbsWS.hs;
      "XMonad/Actions/Backlight.hs" = ./xdg/xmonad/lib/XMonad/Actions/Backlight.hs;
      "XMonad/Actions/FixedWorkspace.hs" = ./xdg/xmonad/lib/XMonad/Actions/FixedWorkspace.hs;
      "XMonad/Actions/Volume.hs" = ./xdg/xmonad/lib/XMonad/Actions/Volume.hs;
      "XMonad/Hooks/EwwLog.hs" = ./xdg/xmonad/lib/XMonad/Hooks/EwwLog.hs;
      "XMonad/Hooks/SmartFloat.hs" = ./xdg/xmonad/lib/XMonad/Hooks/SmartFloat.hs;
      "XMonad/Hooks/TiledProperty.hs" = ./xdg/xmonad/lib/XMonad/Hooks/TiledProperty.hs;
      "XMonad/Layout/TallMastersComboModified.hs" = ./xdg/xmonad/lib/XMonad/Layout/TallMastersComboModified.hs;
    };
  };

  # xsession.windowManager.command = lib.mkForce (
  #   let wmcmd = config.xsession.windowManager.command;
  #       prefix = "env SHLVL=0 ";
  #       suffix = " >/tmp/xmonad.out 2>/tmp/xmonad.err";
  #       finalCmd = "${prefix}${wmcmd}${suffix}";
  #   in finalCmd
  # );

  # let wmcmd = config.xsession.windowManager.command;
  #     commandUntouched = !(lib.strings.hasInfix "SHLVL" wmcmd);
  #     prefix = "env SHLVL=0 ";
  #     suffix = " >/tmp/xmonad.out 2>/tmp/xmonad.err";
  #     finalCmd = lib.mkIf commandUntouched "${prefix}${wmcmd}${suffix}";
  # in finalCmd;
  xsession.windowManager.command = lib.mkForce ''
  export SHLVL=0
  ~/.xmonad/xmonad-x86_64-linux >/tmp/xmonad.out 2>/tmp/xmonad.err
  '';


  home.sessionVariables.WINDOW_MANAGER = "xmonad";
}
