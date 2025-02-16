{ inputs, config, pkgs, lib, ... }:
{
  _module.args = {
    wrapGL = config.lib.nixGL.wrap;
  };

  nixGL.packages = inputs.nixGL.packages;
  nixGL.installScripts = [ "mesa" "nvidiaPrime" ];
}
