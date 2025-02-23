{ pkgs, lib, config, link, ... }:
with lib;
let cfg = config.programs.emacs;
    symlinks = [
      "./secrets.el"
      "./templates"
      "./assets"
      "./init.el"
      "./profile.el"
      "./site-lisp"
      "./user-dict"
      "./custom.el"
      "./preferences.org"
      "./scripts"
      "./snippets"
    ];
in
{
  options = {
    programs.emacs.symlinks = mkOption {
      type = types.listOf types.str;
      default = symlinks;
    };
  };
  config = mkIf cfg.enable {
    # convert to home.file mkOutOfStoreSymlink
    home.file = builtins.listToAttrs (
      lib.lists.forEach (cfg.symlinks) (name:
        {
          name = ".emacs.d/${name}";
          value = {
            source = link "emacs/${name}";
          };
        }
      )
    );

    programs.emacs.package = pkgs.emacs30;

    home.packages = with pkgs; [
      # used by jinx.el
      enchant
      hunspellDicts.en-us
      emacsPackages.jinx

      # node for copilot.el
      nodejs_22

      # used by ox-pandoc
      pandoc
    ];
  };
}
