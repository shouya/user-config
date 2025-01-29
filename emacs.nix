{ pkgs, lib, config, linkConfig, ... }:
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
            source = linkConfig "emacs/${name}";
          };
        }
      )
    );
  };
}
